import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class LoadTest extends FunSuite with ShouldMatchers {

  ignore ("trail options") {
    val start = System.currentTimeMillis
    val opts = Scallop(List("-Ekey1=value1", "key2=value2", "key3=value3"))
      .props[String]('E')
      .trailArg[String]("first list name")
      .trailArg[List[Int]]("first list values")
      .trailArg[String]("second list name")
      .trailArg[List[Double]]("second list values")
      .args(List("first"))
      .args((1 to 100).map(_.toString))
      .args(List("second"))
      .args((1 to 100).map(_.toString))
      .verify
    val end = System.currentTimeMillis
    assert (end - start < 100, "Time bound broken: %d ms" format (end - start))
  }

  ignore ("retrieving options") {
    object Conf extends ScallopConf(List("-a", "1", "-c", "2.0")) {
      val apples = opt[Int]("apples")
      val carrots = opt[Double]("carrots")
      val bananas = 1
    }
    def time(fn: => Int) = {
      val start = System.currentTimeMillis
      var c = 0L
      (1 to 1000000).foreach { i =>
        c += fn
      }
      val end = System.currentTimeMillis
      end - start
    }
    val t = time(Conf.apples())
    assert(t < 100, "Time bound broken: %d ms" format t)
  }

  test ("too many options") {
    val opts = List.fill(10000)(List("-a","1")).flatten
    object Conf extends ScallopConf(opts) {
      val apples = opt[List[Int]]("apples")
    }
    Conf.apples() should have size (10000)
  }

}
