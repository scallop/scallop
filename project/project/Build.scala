import sbt._
object build extends Build {
  override lazy val projects = Seq(root)
  lazy val root = Project("plugins", file(".")).dependsOn(fmppPlugin)
  lazy val fmppPlugin = uri("git://github.com/aloiscochard/xsbt-fmpp-plugin.git")
}
