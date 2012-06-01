import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

/** Playground for new tests (to make better use of test-only) */
class Isolated extends FunSuite with ShouldMatchers {
 
  test ("i") {
    object Conf extends ScallopConf(Seq("-a")) {
      val apples = opt[Boolean]("apples")
    }
    Conf.apples() should equal (true)
  }

}
