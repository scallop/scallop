import sbt._
import Keys._
import fmpp.FmppPlugin._

object build extends Build {
  
  val versRgx = """[0-9]+\.[0-9]+\.[0-9]+""".r
  val readmeVersion = versRgx.findFirstIn(io.Source.fromFile("README.md").getLines.toList.filter(_.contains("libraryDependencies")).mkString).get
  
  val branch = {
    import sys.process._
    "git status" #| Seq("grep", "On branch") #| Seq("sed", "s_\\#\\s*On\\s*branch\\s*__") lines
  }.head.trim
  println("git branch: %s" format branch)
  
  val vers = 
    if (branch == "master")
      readmeVersion
    else {
      val n = readmeVersion.split("\\.")
      (n.init :+ (n.last.toInt + 1)).mkString(".") + "-SNAPSHOT"
    }
  println("version: %s" format vers)
  
  lazy val root = Project("main", file("."),
                          settings = 
                            Defaults.defaultSettings ++
                            fmppSettings ++
                            Seq(version := vers)
                            )
                          .configs(Fmpp)
}
