import AssemblyKeys._

name := "scrollop"

version := "0.1.8"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

seq(Revolver.settings: _*)

seq(jotSettings: _*)

seq(assemblySettings: _*)

resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.7.1" % "test"
)

//mainClass in assembly := Some("main.class")