import sbt._
import Process._
import sbt.Keys._


lazy val solver = (project in file("solver")).
  settings(
    name := "solver",
    version := "1.0",
    scalaVersion := "2.11.4",
    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
    libraryDependencies += "org.clapper" %% "argot" % "1.0.3"
  )
