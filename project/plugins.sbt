
libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-core" % "0.9.5",
  "net.databinder.dispatch" %% "dispatch-tagsoup" % "0.9.5",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "2.3.1.201302201838-r"
)

addSbtPlugin("io.spray" % "sbt-revolver" % "0.6.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.7")

addSbtPlugin("com.github.sbt" %% "sbt-fmpp" % "0.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.0")

