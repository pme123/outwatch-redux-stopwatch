package pme.watch

import outwatch.Sink
import outwatch.dom._
import outwatch.util.Store
import rxscalajs.Observable

import scala.scalajs.js.JSApp

object StopWatchApp extends JSApp {
  def main(): Unit = {
    OutWatch.render("#app", StopWatch().root)
  }
}

case class StopWatch() {

  sealed trait Action

  case object Start extends Action

  case object Stop extends Action

  case object Reset extends Action

  case object Increment extends Action

  case class State(running: Boolean = false, timeInCS: Long = 0)

  private def reducer(previousState: State, action: Action) = {
    println(s"Store reducer: $action")
    action match {
      case Start => previousState.copy(running = true)
      case Stop => previousState.copy(running = false)
      case Reset => State()
      case Increment =>
        if (previousState.running)
          previousState.copy(timeInCS = previousState.timeInCS + 1)
        else
          previousState
    }
  }

  private val store = MyStore(Store(State(), reducer))

  // ticker that sends an Increment Action each 100 milliseconds
  store <-- Observable.interval(100)
    .map(_ => Increment)

  private val startButton = button(
    className := "buttons__start"
    , click(Start) --> store
    , hidden <-- store.map(_.running)
    , "Start"
  )

  private val stopButton = button(
    className := "buttons__stop"
    , click(Stop) --> store
    , hidden <-- store.map(!_.running)
    , "Stop"
  )

  private val resetButton = button(
    className := "buttons__reset"
    , click(Reset) --> store
    , disabled <-- store.map(s => s.running || s.timeInCS == 0)
    , "Reset"
  )

  val root: VNode = div(className := "watch"
    , h1("Outwatch Redux Stopwatch")
    , h2(className := "digits",child <-- store.map(_.timeInCS).map(printWatch))
    , div(className := "buttons"
      , startButton
      , stopButton
      , resetButton)
  )


  private def printWatch(totCenSec: Long) = {
    val cenSec = totCenSec % 10
    val sec = (totCenSec / 10) % 60
    val min = (totCenSec / (10 * 60)) % 60
    val hou = (totCenSec / (10 * 60 * 60)) % 60
    f"$hou%02d:$min%02d.$sec%02d.$cenSec"
  }

}

case class MyStore[State, Action](wrapped: Store[State, Action])

object MyStore {
  implicit def toSink[Action](store: MyStore[_, Action]): Sink[Action] = store.wrapped.sink
  implicit def toSource[State](store: MyStore[State, _]): Observable[State] = store.wrapped.source.share
}
