resolvers ++= Seq(
  "jgit-repo" at "http://download.eclipse.org/jgit/maven"
)

libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-core" % "0.9.5",
  "net.databinder.dispatch" %% "dispatch-tagsoup" % "0.9.5"
)

addSbtPlugin("io.spray" % "sbt-revolver" % "0.6.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.7")

addSbtPlugin("com.github.sbt" %% "sbt-fmpp" % "0.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.0")

