import sbtcrossproject.{CrossType, crossProject}

val versRgx = """[0-9]+\.[0-9]+\.[0-9]+""".r

lazy val commonSettings = Seq(
  organization := "org.rogach",
  name := "scallop",
  version := versRgx.findFirstIn(io.Source.fromFile("README.md").getLines.filter(_.contains("libraryDependencies")).mkString).get,
  scalaVersion := "2.12.0",
  crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.0"),
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:postfixOps",
    "-language:reflectiveCalls",
    "-language:existentials",
    "-language:implicitConversions",
    "-Xlint"
  ),
  licenses := Seq(
    "MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")
  ),
  homepage := Some(url("https://github.com/scallop/scallop")),
  scmInfo := Some(
    ScmInfo(
      browseUrl = url("http://github.com/scallop/scallop"),
      connection = "scm:git:git@github.com:scallop/scallop.git"
    )
  ),
  boilerplateSource in Compile := baseDirectory.value.getParentFile / "shared" / "src" / "main" / "boilerplate",
  pomExtra := (
    <developers>
      <developer>
        <id>clhodapp</id>
        <name>Chris Hodapp</name>
        <url>http://clhodapp.net</url>
      </developer>
      <developer>
        <id>rogach</id>
        <name>Platon Pronko</name>
        <url>http://rogach.org</url>
      </developer>
    </developers>
  ),

  pomIncludeRepository := { x => false },

  publishTo := {
    val snapshot = false
    if (snapshot)
      Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
    else
      Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
  },

  publishMavenStyle := true,

  publishArtifact in Test := false,

  scalacOptions in (Compile, doc) ++= Opts.doc.sourceUrl("https://github.com/scallop/scallop/blob/develop/€{FILE_PATH}.scala"),

  parallelExecution in Test := false,

  git.remoteRepo := "git@github.com:scallop/scallop.git"
)

lazy val scallop =
  crossProject(JVMPlatform, NativePlatform)
    .crossType(CrossType.Full)
    .settings(commonSettings)
    .jvmSettings(
      libraryDependencies ++= Seq(
        "org.scalatest" %%% "scalatest" % "3.0.0" % "test"
      )
    )
    .nativeSettings(
      scalaVersion := "2.11.8",
      crossScalaVersions := "2.11.8" :: Nil
    )

lazy val scallopJVM    = scallop.jvm.enablePlugins(spray.boilerplate.BoilerplatePlugin)
lazy val scallopNative = scallop.native.enablePlugins(spray.boilerplate.BoilerplatePlugin)

lazy val `scala-scallop` =
  (project in file("."))
    .enablePlugins(spray.boilerplate.BoilerplatePlugin)
    .settings(commonSettings, publish := {})
    .aggregate(scallopJVM, scallopNative)

// fix for paths to source files in scaladoc
doc in Compile := {
  Seq("bash","-c",""" for x in $(find target/scala-2.12/api/ -type f); do sed -i "s_`pwd`/__" $x; done """).!
  (doc in Compile).value
}

site.settings

site.includeScaladoc("")

ghpages.settings
