import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class OptionNameGuessing extends FunSuite with ShouldMatchers {
  test ("simple") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      guessOptionName = true
      val apples = opt[Int]()
      val bananas = opt[Int]()
    }
    Conf.apples() should equal (1)
    Conf.bananas.get should equal (None)
  }
  
  test ("tricky") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      guessOptionName = true
      val apples = opt[Int]()
      val applesPlus = apples.map(2+)
      lazy val applesVal = apples()
      val bananas = opt[Int]()
      val aaa = opt[Int]()
    }
    Conf.apples() should equal (1)
    Conf.bananas.get should equal (None)
    Conf.aaa.get should equal (None)
  }
  
  test ("comelCase convert") {
    object Conf extends ScallopConf(Seq("--apple-tree", "1")) {
      guessOptionName = true
      val appleTree = opt[Int]()
    }
    Conf.appleTree() should equal (1)
  }
}
