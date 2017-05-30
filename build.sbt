enablePlugins(ScalaJSPlugin)

name := "outwatch-redux-stopwatch"

version := "0.1.0"

organization := "pme"

scalaVersion := "2.12.1"

jsEnv := PhantomJSEnv().value


libraryDependencies ++= Seq(
  "io.github.outwatch" %%% "outwatch" % "0.9.2"
)

jsDependencies ++= Seq(
)
