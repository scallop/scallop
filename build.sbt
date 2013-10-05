
import org.eclipse.jgit.lib._

organization := "org.rogach"

name := "scallop"

scalaVersion := "2.10.3"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:postfixOps",
  "-language:reflectiveCalls",
  "-language:existentials",
  "-language:implicitConversions",
  "-Xlint",
  "-Ywarn-all"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

val versRgx = """[0-9]+\.[0-9]+\.[0-9]+""".r
val vers = versRgx.findFirstIn(io.Source.fromFile("README.md").getLines.toList.filter(_.contains("libraryDependencies")).mkString).get

unmanagedClasspath in Compile += file("dummy")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

licenses := Seq(
  "MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")
)

homepage := Some(url("https://github.com/Rogach/scallop"))

scmInfo := Some(
  ScmInfo(
    browseUrl = url("http://github.com/Rogach/scallop"),
    connection = "scm:git:git@github.com:Rogach/scallop.git"
  )
)


publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

pomExtra := (
  <developers>
    <developer>
      <id>rogach</id>
      <name>Platon Pronko</name>
      <url>http://rogach.org</url>
    </developer>
  </developers>
)

scalacOptions in (Compile, doc) ++= Opts.doc.sourceUrl("https://github.com/Rogach/scallop/tree/master/€{FILE_PATH}.scala")

parallelExecution in Test := false

site.settings

site.includeScaladoc("")

ghpages.settings

git.remoteRepo := "git@github.com:Rogach/scallop.git"

// fix for paths to source files in scaladoc
doc in Compile := {
  Seq("bash","-c",""" for x in $(find target/scala-2.10/api/ -type f); do sed -i "s_`pwd`/__" $x; done """).!
  (doc in Compile).value
}

lazy val root = Project("main", file("."),
                        settings =
                          Defaults.defaultSettings ++
                          fmppSettings ++
                          Seq(version := vers)
                          ) .configs(Fmpp)
