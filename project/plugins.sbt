libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-core" % "0.9.5",
  "net.databinder.dispatch" %% "dispatch-tagsoup" % "0.9.5",
	"org.eclipse.jgit" % "org.eclipse.jgit.pgm" % "3.0.0.201306101825-r",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "3.0.0.201306101825-r"
)

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8")

addSbtPlugin("com.github.sbt" %% "sbt-fmpp" % "0.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.0")
