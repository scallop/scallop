import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class SubcommandsTest extends FunSuite with ShouldMatchers {
  
  test ("builder") {
    val sub = Scallop()
      .opt[Boolean]("bananas")
    val opts = Scallop()
      .opt[Boolean]("apples")
      .addSubBuilder("tree",sub)
      .args(Seq("-a","tree","-b"))
      .verify
    opts.get[Boolean]("apples") should equal (Some(true))
    opts.get[Boolean]("tree\0bananas") should equal (Some(true))
  }
  
  test ("conf") {
    object Conf extends ScallopConf(Seq("-a", "tree", "-b")) {
      val apples = opt[Boolean]("apples")
      val tree = new ScallopConf(Nil, "tree") {
        val bananas = opt[Boolean]("bananas")
      }
    }
    Conf.apples() should equal (true)
    Conf.tree.bananas() should equal (true)
    // test subcommand retrieval
  }
  
}
