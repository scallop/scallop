import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class ConfTest extends FunSuite with ShouldMatchers {

  test ("full example") {
    object Conf extends ScallopConf(List("-c","3","-E","fruit=apple","7.2")) {
      // all options that are applicable to builder (like description, default, etc) 
      // are applicable here as well
      val count = opt[Int]("count", descr = "count the trees", required = true) 
      val properties = props('E')
      val size = trailArg[Double](required = false)
      verify
    }
    // that's it. Completely type-safe and convenient.
    Conf.count() should equal (3)
    Conf.properties("fruit") should equal (Some("apple"))
    Conf.size.get should equal (Some(7.2))
    // passing into other functions
    def someInternalFunc(conf:Conf.type) {
      conf.count() should equal (3)
    }
    someInternalFunc(Conf)
  }

  test ("simple arg") {
    object Conf extends ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify
    }
    Conf.apples() should equal (3)
  }
  
  test ("prorerty args") {
    object Conf extends ScallopConf(List("-Dkey1=value1", "key2=value2")) {
      val properties = props('D')
      verify
    }
    Conf.properties("key1") should equal (Some("value1"))
    Conf.properties("key2") should equal (Some("value2"))
    Conf.propMap('D') should equal (Map("key1" -> "value1", "key2" -> "value2"))
  }
  
  test ("trailing args") {
    object Conf extends ScallopConf(List("filename1","filename2")) {
      val file1 = trailArg[String]()
      val file2 = trailArg[String](required = false)
      verify
    }
    Conf.file1() should equal ("filename1")
    Conf.file2.get should equal (Some("filename2"))
  }
  
  test ("passing to functions") {
    object Conf extends ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify
    }
    def a(conf:Conf.type) {
      conf.apples.get should equal (Some(3))
    }
    a(Conf)
  }
  
  test ("extracting values before call to verify") {
    intercept[IncompleteBuildException] {
      object Conf extends ScallopConf(List("-a")) {
        val apples = opt[Boolean]("apples").apply()
        verify
      }
      Conf
    }
  }
  
}
