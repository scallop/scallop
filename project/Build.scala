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
  
  def publishSnapshot = Command.command("publish-snapshot") { state =>
    val sonatype = "https://oss.sonatype.org/content/repositories/snapshots"
    val extracted = Project.extract(state)
    val eVersion = extracted.getOpt(version).get
    val crossVersions = extracted.getOpt(crossScalaVersions).getOrElse(Seq(eVersion))

    def getV(i: Int) = "%s~%d~SNAPSHOT" format (eVersion, i)

    val snapshotIndex = {
      import dispatch._
      Http(url("https://oss.sonatype.org/content/repositories/snapshots/org/rogach/scallop_2.9.2/") OK as.tagsoup.NodeSeq).map { x =>
        val vRgx = ("""%s~(\d+)~SNAPSHOT""" format eVersion).r
        x \\ "a" map (_.text) collect { case vRgx(v) => v.toInt}
      }.apply.sorted.lastOption.map(1+).getOrElse(1)
    }

    val snapshotVersion = getV(snapshotIndex)
    printf("Publishing version '%s'\n", snapshotVersion)

    crossVersions.foreach { scalaVers =>
      Project.runTask(
        publish in Compile,
        extracted.append(List(version := snapshotVersion, scalaVersion := scalaVers), state),
        true)
    }

    println("Usage:")
    println("""resolvers += "sonatype snapshots" at "%s"""" format sonatype)
    println("""libraryDependencies += "org.rogach" %%%% "scallop" %% "%s"""" format snapshotVersion)

    state
  }

  lazy val root = Project("main", file("."),
                          settings = 
                            Defaults.defaultSettings ++
                            fmppSettings ++
                            Seq(version := vers)
                            )
                          .configs(Fmpp)
                          .settings(commands += publishSnapshot)
}
