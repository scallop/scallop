
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
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

val versRgx = """[0-9]+\.[0-9]+\.[0-9]+""".r
val vers = versRgx.findFirstIn(io.Source.fromFile("README.md").getLines.toList.filter(_.contains("libraryDependencies")).mkString).get

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
