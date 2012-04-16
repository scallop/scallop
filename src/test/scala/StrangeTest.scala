import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class StrangeTest extends FunSuite with ShouldMatchers {
  
  test("help printing") {
    object Conf extends ScallopConf(List("--help")) {
      val apples = opt[Int]("apples")
      verify
    }
    // Todo - wrap this in security manager preventing&catching exiting
    //Conf
  }
  
}
