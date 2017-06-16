import sbtcrossproject.{CrossType, crossProject}

val versRgx = """[0-9]+\.[0-9]+\.[0-9]+""".r

lazy val commonSettings = Seq(
  organization := "org.rogach",
  name := "scallop",
  version := versRgx.findFirstIn(io.Source.fromFile("README.md").getLines.filter(_.contains("libraryDependencies")).mkString).get,
  scalaVersion := "2.12.2",
  crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.2"),
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
  boilerplateSource in Compile := baseDirectory.value.getParentFile / "src" / "main" / "boilerplate",
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
) ++ site.settings ++ site.includeScaladoc("") ++ ghpages.settings

lazy val scallop =
  crossProject(JVMPlatform, NativePlatform)
  .crossType(new sbtcrossproject.CrossType {
    def projectDir(crossBase: File, projectType: String): File =
      crossBase / projectType
    def projectDir(crossBase: File, platform: sbtcrossproject.Platform): File =
      crossBase / platform.identifier
    def sharedSrcDir(projectBase: File, conf: String): Option[File] =
      Some(projectBase.getParentFile / "src" / conf / "scala")
  })
  .in(file("."))
  .settings(commonSettings)
  .configure(_.enablePlugins(spray.boilerplate.BoilerplatePlugin))
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.0.0" % "test"
    ),
    // fix for paths to source files in scaladoc
    doc in Compile := {
      Seq("bash","-c",""" for x in $(find jvm/target/scala-2.12/api/ -type f); do sed -i "s_`pwd`/__" $x; done """).!
      (doc in Compile).value
    },
    scalacOptions in Test -= "-Xlint"
  )
  .nativeSettings(
    scalaVersion := "2.11.11",
    crossScalaVersions := Seq("2.11.8")
  )

lazy val scallopJVM = scallop.jvm.copy(id = "jvm")
lazy val scallopNative = scallop.native.copy(id = "native")
