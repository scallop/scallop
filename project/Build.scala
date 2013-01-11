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

    def grc(url: String) = { 
      import java.net._;
      printf("Trying: %s\n", url)
      val u = new URL(url)
      val conn = u.openConnection().asInstanceOf[HttpURLConnection]
      conn.setRequestMethod("GET")
      conn.connect()
      conn.getResponseCode()
    }
    def getV(i: Int) = "%s~%d~SNAPSHOT" format (eVersion, i)
    def getUrl(i: Int) = "%s/org/rogach/scallop_2.9.2/%2$s/scallop_2.9.2-%2$s.pom" format (sonatype, getV(i))

    val num = Stream.from(1).find(i => grc(getUrl(i)) == 404).get
    val snapshotVersion = getV(num)
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
