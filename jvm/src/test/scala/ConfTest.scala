package org.rogach.scallop

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class ConfTest extends AnyFunSuite with Matchers with UsefulMatchers with CapturingTest {
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
      verify()
    }
    // that's it. Completely type-safe and convenient.
    Conf.count() should equal (4)
    Conf.properties("fruit") should equal ("apple")
    Conf.size.toOption should equal (Some(7.2))
    // passing into other functions
    def someInternalFunc(conf:Conf.type): Unit = {
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
      val longProperties = propsLong[String]("Props", descr = "more key-value pairs")
      val verbose = opt[Boolean]("verbose", descr = "use more verbose output")
      val amount = opt[Int]("amount", descr = "how many objects do you need?")

      val tree = new Subcommand("tree") {
        val height = opt[Double]("height", descr = "how tall should the tree be?")
        val name = trailArg[String]("tree name", descr = "tree name")
      }
      addSubcommand(tree)

      val palm = new Subcommand("palm") {
        val height = opt[Double]("height", descr = "how tall should the palm be?")
        val name = trailArg[String]("tree name", descr = "palm name")
      }
      addSubcommand(palm)

      verify()
    }
    val (out, err) = captureOutput {
      Conf.builder.printHelp
    }

    out shouldBe """test 1.2.3 (c) 2012 Mr Placeholder
Usage: test [OPTION]... [tree|palm] [OPTION]... [tree-name]
test is an awesome program, which does something funny
Options:

  -a, --amount  <arg>                    how many objects do you need?
  -Dkey=value [key=value]...             some key-value pairs
      --Props key=value [key=value]...   more key-value pairs
  -v, --verbose                          use more verbose output
  -h, --help                             Show help message
      --version                          Show version of this program

Subcommand: tree
  -h, --height  <arg>   how tall should the tree be?
      --help            Show help message

 trailing arguments:
  tree name (required)   tree name
Subcommand: palm
  -h, --height  <arg>   how tall should the palm be?
      --help            Show help message

 trailing arguments:
  tree name (required)   palm name

For all other tricks, consult the documentation!
"""
    err shouldBe ""
  }

  test ("simple arg") {
    object Conf extends ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify()
    }
    Conf.apples() should equal (3)
  }

  test ("prorerty args") {
    object Conf extends ScallopConf(List("-Dkey1=value1", "key2=value2")) {
      val properties = props[String]('D')
      verify()
    }
    Conf.properties.get("key1") should equal (Some("value1"))
    Conf.properties.get("key2") should equal (Some("value2"))
    Conf.properties should equal (Map("key1" -> "value1", "key2" -> "value2"))
  }

  test ("trailing args") {
    object Conf extends ScallopConf(List("filename1","filename2")) {
      val file1 = trailArg[String]()
      val file2 = trailArg[String](required = false)
      verify()
    }
    Conf.file1() should equal ("filename1")
    Conf.file2.toOption should equal (Some("filename2"))
  }

  test ("trailing args - non-required, empty list arg") {
    object Conf extends ScallopConf(Nil) {
      val files = trailArg[List[String]](required = false)
      verify()
    }
    Conf.files.toOption should equal (None)
  }

  test ("trailing args - empty list arg after flag option") {
    object Conf extends ScallopConf(Seq("-v")) {
      val verbose = opt[Boolean]("verbose")
      val files = trailArg[List[String]](required = false)
      verify()
    }
    Conf.verbose() should equal (true)
    Conf.files.toOption should equal (None)
  }

  test ("trailing args - non-empty list arg") {
    object Conf extends ScallopConf(Seq("a")) {
      val files = trailArg[List[String]]()
      verify()
    }
    Conf.files() should equal (List("a"))
  }

  test ("trailing args - non-empty list arg after flag option") {
    object Conf extends ScallopConf(Seq("-v", "a")) {
      val verbose = opt[Boolean]("verbose")
      val files = trailArg[List[String]]()
      verify()
    }
    Conf.verbose() should equal (true)
    Conf.files() should equal (List("a"))
  }

  test ("passing to functions") {
    object Conf extends ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify()
    }
    def a(conf:Conf.type): Unit = {
      conf.apples.toOption should equal (Some(3))
    }
    a(Conf)
  }

  test ("extracting values before call to verify") {
    expectException(IncompleteBuildException()) {
      object Conf extends ScallopConf(List("-a")) {
        val apples = opt[Boolean]("apples").apply()
        verify()
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
      verify()
    }
    Conf.applesCollect.toOption should equal (Some(4))
    Conf.applesFilter1.toOption should equal (Some(3))
    Conf.applesFilter2.toOption should equal (None)
    Conf.applesFilterNot.toOption should equal (Some(3))
    Conf.applesMap1.toOption should equal (Some(5))
    Conf.applesMap2.toOption should equal (None)
    Conf.applesOrElse1.toOption should equal (Some(3))
    Conf.applesOrElse2.toOption should equal (Some(1))
    Conf.bananas.toOption should equal (None)
  }

  test ("printing ScallopOption") {
    object Conf extends ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify()
    }
    Conf.apples.toString should equal ("ScallopSome(3)")
  }

  test ("is supplied - option value was supplied") {
    object Conf extends ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify()
    }
    Conf.apples.isSupplied should equal (true)
  }

  test ("is supplied - option value was not supplied") {
    object Conf extends ScallopConf(Nil) {
      val apples = opt[Int]("apples")
      verify()
    }
    Conf.apples.isSupplied should equal (false)
  }

  test ("toggle flag option") {
    object Conf extends ScallopConf(List("-a")) {
      val apples = opt[Boolean]("apples").map(!_)
      val bananas = opt[Boolean]("bananas").map(!_)
      verify()
    }
    Conf.apples() should equal (false)
    Conf.bananas() should equal (true)
  }

  test ("noshort") {
    object Conf extends ScallopConf(List("-b","1")) {
      val bananas = opt[Int]("bananas", noshort = true)
      val bags = opt[Int]("bags")
      verify()
    }
    Conf.bananas.toOption should equal (None)
    Conf.bags.toOption should equal (Some(1))
  }

  test ("correct validation") {
    object Conf extends ScallopConf(List("-a","1")) {
      val apples = opt[Int]("apples", validate = (0<))
      verify()
    }
    Conf.apples() should equal (1)
  }

  test ("failing validation") {
    expectException(ValidationFailure("Validation failure for 'apples' option parameters: 1")) {
      object Conf extends ScallopConf(List("-a","1")) {
        val apples = opt[Int]("apples", validate = (0>))

      }
      Conf.verify()
    }
  }

  test ("boolean default value") {
    object Conf extends ScallopConf(List("-b")) {
      val apples = opt[Boolean]("apples", default = Some(true))
      val bananas = opt[Boolean]("bananas", default = Some(false))
      verify()
    }
    Conf.apples() should equal (true)
    Conf.bananas() should equal (true)
  }

  test ("custom validation - success") {
    object Conf extends ScallopConf(List("-a","14","-b","3")) {
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      validate (apples, bananas) { (a,b) =>
        if (b > 0 && a % 7 == 0) Right(())
        else Left("Something is wrong with composition :)")
      }

    }
    Conf.verify()
  }

  test ("custom validation - failure") {
    expectException(ValidationFailure("Something is wrong with composition :)")) {
      object Conf extends ScallopConf(List("-a","15","-b","3")) {
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        validate (apples, bananas) { (a,b) =>
          if (b > 0 && a % 7 == 0) Right(())
          else Left("Something is wrong with composition :)")
        }
      }
      Conf.verify()
    }
  }

  test ("custom opt validation - success") {
    object Conf extends ScallopConf(List("-a", "14")) {
      val apples = opt[Int]()
      val bananas = opt[Int]()
      validateOpt (apples, bananas) {
        case (Some(a), None) => Right(())
        case _ => Left("err")
      }
    }
    Conf.verify()
  }

  test ("custom opt validation - failure") {
    expectException(ValidationFailure("err")) {
      object Conf extends ScallopConf(List("-a", "14", "-b", "4")) {
        val apples = opt[Int]()
        val bananas = opt[Int]()
        validateOpt (apples, bananas) {
          case (Some(a), None) => Right(())
          case _ => Left("err")
        }
      }
      Conf.verify()
    }
  }

  test ("validate should fall back to default values if they are present") {
    object Conf extends ScallopConf(List("--start", "7")) {
      val start = opt[Int]("start", default = Some(1))
      val end = opt[Int]("end", default = Some(10))
      validate (start, end) { (s,e) =>
        if (s < e) Right(())
        else Left("Start must be before end")
      }
    }
    Conf.verify()
  }

  test ("validate function should not run if all options are not provided") {
    object Conf extends ScallopConf(List()) {
      val start = opt[Int]("start")
      val end = opt[Int]("end")
      validate (start, end) { (s,e) =>
        if (s <= e) Right(())
        else Left("Start must be before end")
      }

    }
    Conf.verify()
  }

  test ("numbers in option names") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      val apples1 = opt[Int]("apples1")
      val apples2 = opt[Int]("apples2")
      verify()
    }
    Conf.apples1.toOption should equal (Some(1))
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
      verify()
    }
    Conf.weight.toOption should equal (Some(12))
    Conf.weight2.toOption should equal (None)
  }

  test ("short-named property args with commas") {
    object Conf extends ScallopConf(Seq("-Akey1=1,key2=2")) {
      val app = props[Int]('A')
      verify()
    }
    Conf.app("key1") should equal (1)
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("short-named property args with commas and spaces") {
    object Conf extends ScallopConf(Seq("-A","key1=1",",","key2=2")) {
      val app = props[Int]('A')
      verify()
    }
    Conf.app.get("key1") should equal (Some(1))
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("short-named property args with commas and spaces 2") {
    object Conf extends ScallopConf(Seq("-A","key1=1,","key2=2")) {
      val app = props[Int]('A')
      verify()
    }
    Conf.app.get("key1") should equal (Some(1))
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("long-named property args") {
    object Conf extends ScallopConf(Seq("--Apples","key1=1","key2=2")) {
      val app = propsLong[Int]("Apples")
      verify()
    }
    Conf.app.get("key1") should equal (Some(1))
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("long-named property args with commas and spaces") {
    object Conf extends ScallopConf(Seq("--Apples","key1=1",",","key2=2")) {
      val app = propsLong[Int]("Apples")
      verify()
    }
    Conf.app.get("key1") should equal (Some(1))
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("escaped commas in property args") {
    object Conf extends ScallopConf(Seq("-A", "key=1\\,2")) {
      val app = props[String]('A')
      verify()
    }
    Conf.app.get("key") shouldEqual Some("1,2")
  }

  test ("escaped commas mixed with non-escaped separators in property args") {
    object Conf extends ScallopConf(Seq("-A", "key1=1\\,2,key2=3")) {
      val app = props[String]('A')
      verify()
    }
    Conf.app.get("key1") shouldEqual Some("1,2")
    Conf.app.get("key2") shouldEqual Some("3")
  }

  test ("escaped equals in property args") {
    object Conf extends ScallopConf(Seq("-A", "key=1\\=2")) {
      val app = props[String]('A')
      verify()
    }
    Conf.app.get("key") shouldEqual Some("1=2")
  }

  test ("toggle options - positive, long") {
    object Conf extends ScallopConf(Seq("--verbose")) {
      val verbose = toggle("verbose")
      verify()
    }
    Conf.verbose() should equal (true)
    Conf.verbose.isSupplied should equal (true)
  }

  test ("toggle options - negative, long") {
    object Conf extends ScallopConf(Seq("--noverbose")) {
      val verbose = toggle("verbose")
      verify()
    }
    Conf.verbose() should equal (false)
    Conf.verbose.isSupplied should equal (true)
  }

  test ("toggle options - short") {
    object Conf extends ScallopConf(Seq("-v")) {
      val verbose = toggle("verbose")
      verify()
    }
    Conf.verbose() should equal (true)
    Conf.verbose.isSupplied should equal (true)
  }

  test ("toggle options - not supplied") {
    object Conf extends ScallopConf(Seq()) {
      val verbose = toggle("verbose")
      verify()
    }
    Conf.verbose.toOption should equal (None)
    Conf.verbose.isSupplied should equal (false)
  }

  test ("toggle options - not supplied, with default") {
    object Conf extends ScallopConf(Seq()) {
      val verbose = toggle("verbose", default = Some(true))
      verify()
    }
    Conf.verbose.toOption should equal (Some(true))
    Conf.verbose.isSupplied should equal (false)
  }

  test ("forced end of options parsing (--)") {
    object Conf extends ScallopConf(Seq("-a","1","--","-b","2")) {
      val apples = opt[Int]("apples")
      val bananas = trailArg[List[String]]("bananas")
      verify()
    }
    Conf.apples() should equal (1)
    Conf.bananas() should equal (List("-b", "2"))
  }

  test ("help formatter test") {
    val text = Formatter.wrap("supress all output, including output from scripts (stderr from scripts is still printed)".split(" ").toSeq, 76)
    val expected =  (List(
      "supress all output, including output from scripts (stderr from scripts is ",
      "still printed) "
    ))
    text should equal (expected)
  }

  test ("short option with arg concatenation test") {
    object Conf extends ScallopConf(Seq("-ffile")) {
      val file = opt[String]("file")
      verify()
    }
    Conf.file() should equal ("file")
  }

  test ("hyphens as arguments") {
    object Conf extends ScallopConf(Seq("--output", "-", "-")) {
      val output = opt[String]("output")
      val input = trailArg[List[String]]("input")
      verify()
    }
    Conf.output() should equal ("-")
    Conf.input() should equal (List("-"))
  }

  test ("default value of option should be lazily evaluated") {
    val conf = new ScallopConf(Seq("-a", "4")) {
      val apples = opt[Int](default = { sys.error("boom"); None })
      verify()
    }
    conf.apples() should equal (4)
  }

  test ("default value of trailing arg should be lazily evaluated") {
    val conf = new ScallopConf(Seq("4")) {
      val apples = trailArg[Int](default = { sys.error("boom"); None })
      verify()
    }
    conf.apples() should equal (4)
  }

  test ("if no arguments provided for list option, default should be returned") {
    val conf = new ScallopConf(Seq()) {
      val apples = opt[List[Int]](default = Some(List(1)))
      verify()
    }
    conf.apples() should equal (List(1))
  }

  test ("empty tally") {
    val conf = new ScallopConf(Seq()) {
      val apples = tally()
      verify()
    }
    conf.apples() should equal (0)
    conf.apples.isSupplied should equal (false)
  }

  test ("one-arg tally") {
    val conf = new ScallopConf(Seq("-a")) {
      val apples = tally()
      verify()
    }
    conf.apples() should equal (1)
  }

  test ("two-arg tally") {
    val conf = new ScallopConf(Seq("-a", "-a")) {
      val apples = tally()
      verify()
    }
    conf.apples() should equal (2)
  }

  test ("collapsed two-arg tally") {
    val conf = new ScallopConf(Seq("-aa")) {
      val apples = tally()
      verify()
    }
    conf.apples() should equal (2)
  }

  test ("tally no-args") {
    expectException(WrongOptionFormat("apples", "stuff", "this option doesn't need arguments")) {
      val conf = new ScallopConf(Seq("-a", "stuff", "--verbose")) {
        val apples = tally()
        val verbose = opt[Boolean]()
        verify()
      }
    }
  }

  test ("empty list arg before empty trailing option") {
    val conf = new ScallopConf(Seq("-a")) {
      val apples = opt[List[String]](default = Some(Nil))
      verify()
    }
    conf.apples() ==== List()
  }

  test ("multiple list option before normal option should keep ordering") {
    val conf = new ScallopConf(Seq("-l", "1", "-l", "2", "-o", "3")) {
      val l = opt[List[String]]()
      val o = opt[Int]()
      verify()
    }
    conf.l() ==== List("1","2")
  }

  test ("multiple list option before optional trail arg should keep ordering") {
    val conf = new ScallopConf(Seq("-l", "1", "-l", "2", "--", "0")) {
      val l = opt[List[String]]()
      val t = trailArg[Int](required = false)
      verify()
    }
    conf.l() ==== List("1","2")
  }

  test ("verification on subconfigs") {
    expectException(WrongOptionFormat("apples", "b", "bad Int value")) {
      val conf = new ScallopConf(Seq("tree", "-a", "b")) {
        val tree = new Subcommand("tree") {
          val apples = opt[Int]()
        }
        addSubcommand(tree)

        verify()
      }
    }
  }

  test ("validation failure on subconfigs") {
    expectException(ValidationFailure("tree: a + b must be < 3")) {
      val conf = new ScallopConf(Seq("tree", "-a", "1", "-b", "5")) {
        val tree = new Subcommand("tree") {
          val apples = opt[Int]()
          val bananas = opt[Int]()
          validate(apples, bananas) { (a, b) =>
            if (a + b >= 3) Left("tree: a + b must be < 3")
            else Right(())
          }
        }
        addSubcommand(tree)

        verify()
      }
    }
  }

  test ("validation failure on nested subconfigs") {
    expectException(ValidationFailure("branch: a + b must be < 3")) {
      val conf = new ScallopConf(Seq("tree", "branch", "-a", "1", "-b", "5")) {
        val tree = new Subcommand("tree") {
          val branch = new Subcommand("branch") {
            val apples = opt[Int]()
            val bananas = opt[Int]()
            validate(apples, bananas) { (a, b) =>
              if (a + b >= 3) Left("branch: a + b must be < 3")
              else Right(())
            }
          }
          addSubcommand(branch)
        }
        addSubcommand(tree)

        verify()
      }
    }
  }

  test ("validationOpt failure on subconfigs") {
    expectException(ValidationFailure("both a and b must be supplied")) {
      val conf = new ScallopConf(Seq("tree", "-a", "1")) {
        val tree = new Subcommand("tree") {
          val apples = opt[Int]()
          val bananas = opt[Int]()
          validateOpt(apples, bananas) {
            case (Some(a), Some(b)) => Right(())
            case _ => Left("both a and b must be supplied")
          }
        }
        addSubcommand(tree)

        verify()
      }
    }
  }

  test ("using mixins") {
    class CommonOptions(args: Seq[String]) extends ScallopConf(args) {
      val fruits = opt[Int]()
    }

    trait AppleOption { self: ScallopConf =>
      val apples = opt[Int]()
    }

    trait BananaOption { self: ScallopConf =>
      val bananas = opt[Int]()
    }

    class FooOptions(args: Seq[String]) extends CommonOptions(args) with AppleOption
    class BarOptions(args: Seq[String]) extends CommonOptions(args) with AppleOption with BananaOption

    val foo = new FooOptions(Seq("--apples", "42"))
    foo.verify()
    foo.apples() shouldBe 42

    val bar = new BarOptions(Seq("--apples", "42", "--bananas", "43"))
    bar.verify()
    bar.apples() shouldBe 42
    bar.bananas() shouldBe 43
  }

  test ("--arg=value option style") {
    val conf = new ScallopConf(Seq("--apples=42")) {
      val apples = opt[Int]()
      verify()
    }

    conf.apples() shouldBe 42
  }

  test ("pass arguments that start with dash") {
    val conf = new ScallopConf(Seq("--apples=-1")) {
      val apples = opt[Int]()
      verify()
    }

    conf.apples() shouldBe (-1)
  }

  test ("pass list of arguments in --arg=value option style") {
    val conf = new ScallopConf(Seq("--apples=-1", "--apples=-2")) {
      val apples = opt[List[Int]]()
      verify()
    }

    conf.apples() shouldBe List(-1, -2)
  }

  test ("handle trailing args in conjunction with --arg=value option style") {
    val conf = new ScallopConf(Seq("--apples=-1", "basket")) {
      val apples = opt[Int]()
      val stuff = trailArg[String]()
      verify()
    }

    conf.apples() shouldBe (-1)
    conf.stuff() shouldBe "basket"
  }

  test ("accessing unverified builder from default option value resolver") {
    intercept[Help] {
      class Conf (arguments: Seq[String]) extends ScallopConf(arguments) {
        appendDefaultToDescription = true
        val protocol = opt[String](default = Some("http"))
        val port = opt[Int](default = protocol() match {
          case "http" => Some(80)
          case _ => None
        })
        verify()
      }

      val conf = new Conf(Seq("--help"))
    }
  }

  test ("default option value resolver accessing other options") {
    class Conf (arguments: Seq[String]) extends ScallopConf(arguments) {
      val protocol = opt[String](name = "protocol", noshort = true, required = false, default = Some("http"))
      val port = opt[Int](name = "port", noshort = true, default = protocol() match {
        case "http" => Some(80)
        case _ => None
      })
      verify()
    }

    val conf = new Conf(Seq())
    conf.protocol() shouldEqual "http"
    conf.port() shouldEqual 80
  }

  test ("isSupplied on transformed option with guessed option name") {
    val config = new ScallopConf(Seq("-i", "5")) {
      val index = opt[Int]().map(_-1)
      verify()
    }
    config.index.isSupplied shouldEqual true
  }

  test ("isSupplied on transformed option with guessed option name inside validation") {
    val config = new ScallopConf(Seq("-i", "5", "-l", "10")) {
      val index = opt[Int]().map(_-1)
      val length = opt[Int]()
      addValidation {
        if (index.isSupplied && length.isSupplied && index() >= length()) {
          Left("Index out of bounds")
        } else Right(())
      }
      verify()
    }
    config.index.isSupplied shouldEqual true
  }

  test ("negative number in trailing arguments") {
    val config = new ScallopConf(Seq("-1234")) {
      val value = trailArg[Int]()
      verify()
    }
    config.value() shouldEqual -1234
  }

}
