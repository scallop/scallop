import sbtcrossproject.crossProject

lazy val commonSettings = Seq(
  organization := "org.rogach",
  name := "scallop",
  version := {
    val versionRegexp = """[0-9]+\.[0-9]+\.[0-9]+""".r
    val libraryDependenciesString =
      scala.io.Source.fromFile("README.md").getLines.filter(_.contains("libraryDependencies")).mkString
    versionRegexp.findFirstIn(libraryDependenciesString).get
  },
  scalaVersion := "2.12.8",
  crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.8", "2.13.0-RC3"),
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
  unmanagedSourceDirectories in Compile += {
    val base = baseDirectory.value.getParentFile / "src" / "main"
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v >= 13 =>
        base / s"scala-2.13+"
      case _ =>
        base / s"scala-2.13-"
    }
  },
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
  siteSubdirName in SiteScaladoc := "",
  git.remoteRepo := "git@github.com:scallop/scallop.git"
)

lazy val scallop =
  crossProject(JVMPlatform, NativePlatform, JSPlatform)
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
  .enablePlugins(SiteScaladocPlugin, GhpagesPlugin)
  .configure(_.enablePlugins(spray.boilerplate.BoilerplatePlugin))
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.0.8-RC5" % Test
    ),
    // fix for paths to source files in scaladoc
    doc in Compile := {
      import sys.process._
      Seq("bash","-c",""" for x in $(find jvm/target/scala-2.12/api/ -type f); do sed -i "s_`pwd`/__" $x; done """).!
      (doc in Compile).value
    },
    scalacOptions in Test -= "-Xlint"
  )
  .nativeSettings(
    scalaVersion := "2.11.12",
    crossScalaVersions := Seq("2.11.12")
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer in Test := true
  )
