import AssemblyKeys._

organization := "org.rogach"

name := "scallop"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-deprecation", "-unchecked")

seq(Revolver.settings: _*)

seq(assemblySettings: _*)

resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

crossScalaVersions := Seq("2.9.0", "2.9.0-1", "2.9.1", "2.9.1-1", "2.9.2", "2.9.3")

unmanagedClasspath in Compile += file("dummy")

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

pomExtra := (
  <url>https://github.com/Rogach/scallop</url>
  <licenses>
    <license>
      <name>MIT License</name>
      <url>http://www.opensource.org/licenses/mit-license.php</url>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:Rogach/scallop.git</url>
    <connection>scm:git:git@github.com:Rogach/scallop.git</connection>
  </scm>
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
doc in Compile <<= (doc in Compile) map { in =>
  Seq("bash","-c",""" for x in $(find target/scala-2.9.2/api/ -type f); do sed -i "s_`pwd`/__" $x; done """).!
  in
}
