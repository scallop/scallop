import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class ConfTest extends FunSuite with ShouldMatchers {

  test ("full example") {
    object Conf extends ScallopConf(List("-c","3","-E","fruit=apple","7.2")) {
      // all options that are applicable to builder (like description, default, etc) 
      // are applicable here as well
      val count:ScallopOption[Int] = opt[Int]("count", descr = "count the trees", required = true)
                    .map(1+) // also here work all standard Option methods -
                             // evaluation is deferred to after option construcnion
      val properties = props[String]('E')
      // types (:ScallopOption[Double]) can be omitted, here just for clarity
      val size:ScallopOption[Double] = trailArg[Double](required = false)
      verify
    }
    // that's it. Completely type-safe and convenient.
    Conf.count() should equal (4)
    Conf.properties("fruit") should equal (Some("apple"))
    Conf.size.get should equal (Some(7.2))
    // passing into other functions
    def someInternalFunc(conf:Conf.type) {
      conf.count() should equal (4)
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
      val properties = props[String]('D')
      verify
    }
    Conf.properties("key1") should equal (Some("value1"))
    Conf.properties("key2") should equal (Some("value2"))
    Conf.propMap[String]('D') should equal (Map("key1" -> "value1", "key2" -> "value2"))
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
  
  test ("option operations - collect") {
    object Conf extends ScallopConf(List("-a","3","-b","5")) {
      val apples = opt[Int]("apples")
      val applesCollect = apples.collect({case a:Int => a + 1})
      val applesFilter1 = apples.filter(2<)
      val applesFilter2 = apples.filter(5<)
      val applesFilterNot = apples.filterNot(5<)
      val applesMap1 = apples.map(2+)
      val applesMap2 = apples.filter(5<).map(2+)
      val applesOrElse1 = apples.orElse(Some(1))
      val applesOrElse2 = apples.filter(5<).orElse(Some(1))
      val bananas = opt[String]("bananas").collect({case b:Int => b + 1}:PartialFunction[Any,Int])
      verify
    }
    Conf.applesCollect.get should equal (Some(4))
    Conf.applesFilter1.get should equal (Some(3))
    Conf.applesFilter2.get should equal (None)
    Conf.applesFilterNot.get should equal (Some(3))
    Conf.applesMap1.get should equal (Some(5))
    Conf.applesMap2.get should equal (None)
    Conf.applesOrElse1.get should equal (Some(3))
    Conf.applesOrElse2.get should equal (Some(1))
    Conf.bananas.get should equal (None)
  }
  
  test ("printing ScallopOption") {
    object Conf extends ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify
    }
    Conf.apples.toString should equal ("ScallopSome(3)")
  }
  
  test ("is supplied - option value was supplied") {
    object Conf extends ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify
    }
    Conf.apples.isSupplied should equal (true)
  }
  
  test ("is supplied - option value was not supplied") {
    object Conf extends ScallopConf(Nil) {
      val apples = opt[Int]("apples")
      verify
    }
    Conf.apples.isSupplied should equal (false)
  }
  
  test ("toggle flag option") {
    object Conf extends ScallopConf(List("-a")) {
      val apples = opt[Boolean]("apples").map(!_)
      val bananas = opt[Boolean]("bananas").map(!_)
      verify
    }
    Conf.apples() should equal (false)
    Conf.bananas() should equal (true)
  }

  test ("noshort") {
    object Conf extends ScallopConf(List("-b","1")) {
      val bananas = opt[Int]("bananas", noshort = true)
      val bags = opt[Int]("bags")
      verify
    }
    Conf.bananas.get should equal (None)
    Conf.bags.get should equal (Some(1))
  }
  
  test ("correct validation") {
    object Conf extends ScallopConf(List("-a","1")) {
      val apples = opt[Int]("apples", validate = (0<))
      verify
    }
    Conf.apples() should equal (1)
  }
 
  test ("failing validation") {
    intercept[ValidationFailure] {
      object Conf extends ScallopConf(List("-a","1")) {
        val apples = opt[Int]("apples", validate = (0>))
        verify
      }
      Conf
    }
  }
  
  test ("option set validation, mutually exclusive options, success") { 
    object Conf extends ScallopConf(List("-a","1")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      mutuallyExclusive(apples, bananas)
      verify
    }
    Conf
  }

  test ("option set validation, mutually exclusive options, failure") { 
    intercept[OptionSetValidationFailure] {
      object Conf extends ScallopConf(List("-a", "1", "-b", "2")){
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        mutuallyExclusive(apples, bananas)
        verify
      }
      Conf
    }
  }

  test ("option set validation, codependent options, success") { 
    object Conf extends ScallopConf(List("-a","1","-b","2")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      codependent(apples, bananas)
      verify
    }
    Conf
  }

  test ("option set validation, codependent options, failure") { 
    intercept[OptionSetValidationFailure] {
      object Conf extends ScallopConf(List("-a", "1")){
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        codependent(apples, bananas)
        verify
      }
      Conf
    }
  }
  
}
