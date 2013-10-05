
addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.2")

val fmpp = uri("https://github.com/sbt/sbt-fmpp.git#release-0.3")

val root = Project("root", file(".")).dependsOn(fmpp)

