import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class OptionNameGuessing extends FunSuite with ShouldMatchers {
  test ("simple") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      guessOptionName = true
      val appleso = opt[Int]()
      val bananaso = opt[Int]()
    }
    Conf.appleso() should equal (1)
    Conf.bananaso.get should equal (None)
  }
  
  test ("tricky") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      guessOptionName = true
      val appleso = opt[Int]()
      val applesPlus = appleso.map(2+)
      lazy val applesVal = appleso()
      val bananaso = opt[Int]()
      val aaa = opt[Int]()
    }
    Conf.appleso() should equal (1)
    Conf.bananaso.get should equal (None)
    Conf.aaa.get should equal (None)
  }
  
  test ("comelCase convert") {
    object Conf extends ScallopConf(Seq("--apple-treeo", "1")) {
      guessOptionName = true
      val appleTreeo = opt[Int]()
    }
    Conf.appleTreeo() should equal (1)
  }
}
