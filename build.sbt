
organization := "org.rogach"

name := "scallop"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

crossScalaVersions := Seq("2.9.0", "2.9.0-1", "2.9.1", "2.9.1-1", "2.9.2", "2.9.3")

unmanagedClasspath in Compile += file("dummy")

val versRgx = """[0-9]+\.[0-9]+\.[0-9]+""".r

version := versRgx.findFirstIn(io.Source.fromFile("README.md").getLines.filter(_.contains("libraryDependencies")).mkString).get

licenses := Seq(
  "MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")
)

homepage := Some(url("https://github.com/scallop/scallop"))

scmInfo := Some(
  ScmInfo(
    browseUrl = url("http://github.com/scallop/scallop"),
    connection = "scm:git:git@github.com:scallop/scallop.git"
  )
)

pomExtra := (
  <developers>
    <developer>
      <id>clhodapp</id>
      <name>Chris Hodapp</name>
      <url>http:/clhodapp.net</url>
    </developer>
    <developer>
      <id>rogach</id>
      <name>Platon Pronko</name>
      <url>http://rogach.org</url>
    </developer>
  </developers>
)

scalacOptions in (Compile, doc) ++= Opts.doc.sourceUrl("https://github.com/scallop/scallop/tree/master/€{FILE_PATH}.scala")

parallelExecution in Test := false

site.settings

site.includeScaladoc("")

ghpages.settings

git.remoteRepo := "git@github.com:scallop/scallop.git"

// fix for paths to source files in scaladoc
doc in Compile <<= (doc in Compile) map { in =>
  Seq("bash","-c",""" for x in $(find target/scala-2.9.2/api/ -type f); do sed -i "s_`pwd`/__" $x; done """).!
  in
}

fmppSettings

lazy val root = project.in(file(".")).configs(Fmpp)
