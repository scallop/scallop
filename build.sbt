import AssemblyKeys._

name := "scrollop"

version := "0.1.13"

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

crossScalaVersions := Seq("2.9.0", "2.9.0-1", "2.9.1")

unmanagedClasspath in Compile += file("dummy")
