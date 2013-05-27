package org.rogach.scallop

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class ConfTest extends FunSuite with ShouldMatchers with UsefulMatchers {
  throwError.value = true

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
    }
    // that's it. Completely type-safe and convenient.
    Conf.count() should equal (4)
    Conf.properties("fruit") should equal ("apple")
    Conf.size.get should equal (Some(7.2))
    // passing into other functions
    def someInternalFunc(conf:Conf.type) {
      conf.count() should equal (4)
    }
    someInternalFunc(Conf)
  }

  test ("output help") {
    object Conf extends ScallopConf(Seq()) {
      version("test 1.2.3 (c) 2012 Mr Placeholder")
      banner("""Usage: test [OPTION]... [tree|palm] [OPTION]... [tree-name]
               |test is an awesome program, which does something funny
               |Options:
               |""".stripMargin)
      footer("\nFor all other tricks, consult the documentation!")
      // ... options ...
      val properties = props[String]('D', descr = "some key-value pairs")
      val verbose = opt[Boolean]("verbose", descr = "use more verbose output")
      val amount = opt[Int]("amount", descr = "how many objects do you need?")
      val tree = new Subcommand("tree") {
        val height = opt[Double]("height", descr = "how tall should the tree be?")
        val name = trailArg[String]("tree name", descr = "tree name")
      }
      val palm = new Subcommand("palm") {
        val height = opt[Double]("height", descr = "how tall should the palm be?")
        val name = trailArg[String]("tree name", descr = "palm name")
      }
    }
    Conf.builder.printHelp
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
    Conf.properties.get("key1") should equal (Some("value1"))
    Conf.properties.get("key2") should equal (Some("value2"))
    Conf.properties should equal (Map("key1" -> "value1", "key2" -> "value2"))
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

  test ("trailing args - non-required, empty list arg") {
    object Conf extends ScallopConf(Nil) {
      val files = trailArg[List[String]](required = false)
    }
    Conf.files.get should equal (None)
  }

  test ("trailing args - empty list arg after flag option") {
    object Conf extends ScallopConf(Seq("-v")) {
      val verbose = opt[Boolean]("verbose")
      val files = trailArg[List[String]](required = false)
    }
    Conf.verbose() should equal (true)
    Conf.files.get should equal (None)
  }

  test ("trailing args - non-empty list arg") {
    object Conf extends ScallopConf(Seq("a")) {
      val files = trailArg[List[String]]()
    }
    Conf.files() should equal (List("a"))
  }

  test ("trailing args - non-empty list arg after flag option") {
    object Conf extends ScallopConf(Seq("-v", "a")) {
      val verbose = opt[Boolean]("verbose")
      val files = trailArg[List[String]]()
    }
    Conf.verbose() should equal (true)
    Conf.files() should equal (List("a"))
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

  test ("mutually exclusive flag options - validation success") {
    object Conf extends ScallopConf(List("-a")) {
      val apples = opt[Boolean]("apples")
      val bananas = opt[Boolean]("bananas")
      mutuallyExclusive(apples,bananas)
      verify
    }
    Conf
  }

  test ("mutually exclusive flag options - validation failure") {
    intercept[OptionSetValidationFailure] {
      object Conf extends ScallopConf(List("-a", "-b")) {
        val apples = opt[Boolean]("apples")
        val bananas = opt[Boolean]("bananas")
        mutuallyExclusive(apples,bananas)
        verify
      }
      Conf
    }
  }

  test ("boolean default value") {
    object Conf extends ScallopConf(List("-b")) {
      val apples = opt[Boolean]("apples", default = Some(true))
      val bananas = opt[Boolean]("bananas", default = Some(false))
      verify
    }
    Conf.apples() should equal (true)
    Conf.bananas() should equal (true)
  }

  test ("custom validation - success") {
    object Conf extends ScallopConf(List("-a","14","-b","3")) {
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      validate (apples, bananas) { (a,b) =>
        if (b > 0 && a % 7 == 0) Right(Unit)
        else Left("Something is wrong with composition :)")
      }
      verify
    }
    Conf
  }

  test ("custom validation - failure") {
    intercept[ValidationFailure] {
      object Conf extends ScallopConf(List("-a","15","-b","3")) {
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        validate (apples, bananas) { (a,b) =>
          if (b > 0 && a % 7 == 0) Right(Unit)
          else Left("Something is wrong with composition :)")
        }
        verify
      }
      Conf
    }
  }

  test ("custom opt validation - success") {
    object Conf extends ScallopConf(List("-a", "14")) {
      val apples = opt[Int]()
      val bananas = opt[Int]()
      validateOpt (apples, bananas) {
        case (Some(a), None) => Right(Unit)
        case _ => Left("err")
      }
    }
    Conf
  }

  test ("custom opt validation - failure") {
    intercept[ValidationFailure] {
      object Conf extends ScallopConf(List("-a", "14", "-b", "4")) {
        val apples = opt[Int]()
        val bananas = opt[Int]()
        validateOpt (apples, bananas) {
          case (Some(a), None) => Right(Unit)
          case _ => Left("err")
        }
      }
      Conf
    }
  }

  test ("numbers in option names") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      val apples1 = opt[Int]("apples1")
      val apples2 = opt[Int]("apples2")
      verify
    }
    Conf.apples1.get should equal (Some(1))
  }

  test ("for comprehensions for ScallopOptions") {
    object Conf extends ScallopConf(Seq("-a","3","-b","2")) {
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val weight = for {
        a <- apples
        if a > 2
        b <- bananas
      } yield a * 2 + b * 3
      val weight2 = for { a <- apples; if a < 2; b <- bananas } yield a * 2 + b * 3
      verify
    }
    Conf.weight.get should equal (Some(12))
    Conf.weight2.get should equal (None)
  }

  test ("automatic verification") {
    object Conf extends ScallopConf(Seq("-a","1")) {
      val apples = opt[Int]("apples")
    }
    Conf.verified should equal (true)
    Conf.apples() should equal (1)
  }

  test ("automatic verification, with deeper hierarcy") {
    class AppleConf(args:Seq[String]) extends ScallopConf(args) {
      val apples = opt[Int]("apples")
    }
    object Conf extends AppleConf(Seq("-a","1","-b","3")) {
      val bananas = opt[Int]("bananas")
    }
    Conf.verified should equal (true)
    Conf.apples() should equal (1)
    Conf.bananas() should equal (3)
  }

  test ("short-named property args with commas") {
    object Conf extends ScallopConf(Seq("-Akey1=1,key2=2")) {
      val app = props[Int]('A')
    }
    Conf.app("key1") should equal (1)
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("short-named property args with commas and spaces") {
    object Conf extends ScallopConf(Seq("-A","key1=1",",","key2=2")) {
      val app = props[Int]('A')
    }
    Conf.app.get("key1") should equal (Some(1))
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("short-named property args with commas and spaces 2") {
    object Conf extends ScallopConf(Seq("-A","key1=1,","key2=2")) {
      val app = props[Int]('A')
    }
    Conf.app.get("key1") should equal (Some(1))
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("long-named property args") {
    object Conf extends ScallopConf(Seq("--Apples","key1=1","key2=2")) {
      val app = propsLong[Int]("Apples")
    }
    Conf.app.get("key1") should equal (Some(1))
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("long-named property args with commas and spaces") {
    object Conf extends ScallopConf(Seq("--Apples","key1=1",",","key2=2")) {
      val app = propsLong[Int]("Apples")
    }
    Conf.app.get("key1") should equal (Some(1))
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("toggle options - positive, long") {
    object Conf extends ScallopConf(Seq("--verbose")) {
      val verbose = toggle("verbose")
    }
    Conf.verbose() should equal (true)
    Conf.verbose.isSupplied should equal (true)
  }

  test ("toggle options - negative, long") {
    object Conf extends ScallopConf(Seq("--noverbose")) {
      val verbose = toggle("verbose")
    }
    Conf.verbose() should equal (false)
    Conf.verbose.isSupplied should equal (true)
  }

  test ("toggle options - short") {
    object Conf extends ScallopConf(Seq("-v")) {
      val verbose = toggle("verbose")
    }
    Conf.verbose() should equal (true)
    Conf.verbose.isSupplied should equal (true)
  }

  test ("toggle options - not supplied") {
    object Conf extends ScallopConf(Seq()) {
      val verbose = toggle("verbose")
    }
    Conf.verbose.get should equal (None)
    Conf.verbose.isSupplied should equal (false)
  }

  test ("toggle options - not supplied, with default") {
    object Conf extends ScallopConf(Seq()) {
      val verbose = toggle("verbose", default = Some(true))
    }
    Conf.verbose.get should equal (Some(true))
    Conf.verbose.isSupplied should equal (false)
  }

  test ("forced end of options parsing (--)") {
    object Conf extends ScallopConf(Seq("-a","1","--","-b","2")) {
      val apples = opt[Int]("apples")
      val bananas = trailArg[List[String]]("bananas")
    }
    Conf.apples() should equal (1)
    Conf.bananas() should equal (List("-b", "2"))
  }

  test ("help formatter test") {
    val text = Formatter.wrap("supress all output, including output from scripts (stderr from scripts is still printed)".split(" "), 76)
    val expected =  (List(
      "supress all output, including output from scripts (stderr from scripts is ",
      "still printed) "
    ))
    text should equal (expected)
  }

  test ("sort option with arg concatenation test") {
    object Conf extends ScallopConf(Seq("-ffile")) {
      val file = opt[String]("file")
    }
    Conf.file() should equal ("file")
  }

  test ("hyphens as arguments") {
    object Conf extends ScallopConf(Seq("--output", "-", "-")) {
      val output = opt[String]("output")
      val input = trailArg[List[String]]("input")
    }
    Conf.output() should equal ("-")
    Conf.input() should equal (List("-"))
  }

  test ("default value of option should be lazily evaluated") {
    val conf = new ScallopConf(Seq("-a", "4")) {
      val apples = opt[Int](default = { sys.error("boom"); None })
    }
    conf.apples() should equal (4)
  }

  test ("default value of trailing arg should be lazily evaluated") {
    val conf = new ScallopConf(Seq("4")) {
      val apples = trailArg[Int](default = { sys.error("boom"); None })
    }
    conf.apples() should equal (4)
  }

  test ("if no arguments provided for list option, default should be returned") {
    val conf = new ScallopConf(Seq()) {
      val apples = opt[List[Int]](default = Some(List(1)))
    }
    conf.apples() should equal (List(1))
  }

  test ("empty tally") {
    val conf = new ScallopConf(Seq()) {
      val apples = tally()
    }
    conf.apples() should equal (0)
    conf.apples.isSupplied should equal (false)
  }

  test ("one-arg tally") {
    val conf = new ScallopConf(Seq("-a")) {
      val apples = tally()
    }
    conf.apples() should equal (1)
  }

  test ("two-arg tally") {
    val conf = new ScallopConf(Seq("-a", "-a")) {
      val apples = tally()
    }
    conf.apples() should equal (2)
  }

  test ("collapsed two-arg tally") {
    val conf = new ScallopConf(Seq("-aa")) {
      val apples = tally()
    }
    conf.apples() should equal (2)
  }

  test ("tally no-args") {
    intercept[WrongOptionFormat] {
      val conf = new ScallopConf(Seq("-a", "stuff", "--verbose")) {
        val apples = tally()
        val verbose = opt[Boolean]()
      }
    }
  }

  test ("empty list arg before empty trailing option") {
    val conf = new ScallopConf(Seq("-a")) {
      val apples = opt[List[String]](default = Some(Nil))
    }
    conf.apples() ==== List()
  }

  test ("multiple list option before normal option should keep ordering") {
    val conf = new ScallopConf(Seq("-l", "1", "-l", "2", "-o", "3")) {
      val l = opt[List[String]]()
      val o = opt[Int]()
    }
    conf.l() ==== List("1","2")
  }

  test ("multiple list option before optional trail arg should keep ordering") {
    val conf = new ScallopConf(Seq("-l", "1", "-l", "2", "--", "0")) {
      val l = opt[List[String]]()
      val t = trailArg[Int](required = false)
    }
    conf.l() ==== List("1","2")
  }

  test ("verification on subconfigs") {
    intercept[WrongOptionFormat] {
      val conf = new ScallopConf(Seq("tree", "-a", "b")) {
        val tree = new Subcommand("tree") {
          val apples = opt[Int]()
        }
      }
    }
  }

  test ("validation failure on subconfigs") {
    intercept[ValidationFailure] {
      val conf = new ScallopConf(Seq("tree", "-a", "1", "-b", "5")) {
        val tree = new Subcommand("tree") {
          val apples = opt[Int]()
          val bananas = opt[Int]()
          validate(apples, bananas) { (a, b) =>
            if (a + b >= 3) Left("tree: a + b must be < 3")
            else Right(Unit)
          }
        }
      }
    }
  }

  test ("validationOpt failure on subconfigs") {
    intercept[ValidationFailure] {
      val conf = new ScallopConf(Seq("tree", "-a", "1")) {
        val tree = new Subcommand("tree") {
          val apples = opt[Int]()
          val bananas = opt[Int]()
          validateOpt(apples, bananas) {
            case (Some(a), Some(b)) => Right(Unit)
            case _ => Left("both a and b must be supplied")
          }
        }
      }
    }
  }

}
