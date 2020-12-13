package org.rogach.scallop

import org.rogach.scallop.exceptions._

/* Tests for examples in the wiki */
class WikiTest extends ScallopTestBase {

  test ("Arguments validation: depending on any option in a list") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      val apples = opt[Int]()
      val bananas = opt[Int]()
      val coconuts = opt[Int]()
      dependsOnAny(apples, List(bananas, coconuts))
      verify()
    }

    intercept[ValidationFailure] {
      new Conf(Seq("--apples", "1"))
    }
    new Conf(Seq("--apples", "1", "--bananas", "2"))
    new Conf(Seq("--apples", "1", "--coconuts", "3"))
  }

  test ("Arguments validation: depending on all options in a list") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      val apples = opt[Int]()
      val bananas = opt[Int]()
      val coconuts = opt[Int]()
      dependsOnAll(apples, List(bananas, coconuts))
      verify()
    }

    intercept[ValidationFailure] {
      new Conf(Seq("--apples", "1"))
    }
    intercept[ValidationFailure] {
      new Conf(Seq("--apples", "1", "--bananas", "2"))
    }
    intercept[ValidationFailure] {
      new Conf(Seq("--apples", "1", "--coconuts", "3"))
    }
    new Conf(Seq("--apples", "1", "--bananas", "2", "--coconuts", "3"))
  }

  test ("Arguments validation: conflicting options") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      val apples = opt[Int]()
      val bananas = opt[Int]()
      val coconuts = opt[Int]()
      conflicts(apples, List(bananas, coconuts))
      verify()
    }

    new Conf(Seq("--apples", "1"))
    intercept[ValidationFailure] {
      new Conf(Seq("--apples", "1", "-b", "2"))
    }
    new Conf(Seq("-b", "2"))
    intercept[ValidationFailure] {
      new Conf(Seq("--apples", "1", "-c", "3"))
    }
    new Conf(Seq("-c", "3"))
  }

  test ("Arguments validation: mutually exclusive") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      val apples = opt[Int]()
      val bananas = opt[Int]()
      mutuallyExclusive(apples, bananas)
      verify()
    }

    new Conf(Seq("-a", "1"))
    new Conf(Seq("-b", "2"))
    intercept[ValidationFailure] {
      new Conf(Seq("-a", "1", "-b", "2"))
    }
  }

  test ("Arguments validation: exactly one") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      val apples = opt[Int]()
      val bananas = opt[Int]()
      requireOne(apples, bananas)
      verify()
    }

    intercept[ValidationFailure] {
      new Conf(Seq())
    }
    new Conf(Seq("-a", "1"))
    new Conf(Seq("-b", "1"))
    intercept[ValidationFailure] {
      new Conf(Seq("-a", "1", "-b", "2"))
    }
  }

  test ("Arguments validation: codependent") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      val apples = opt[Int]()
      val bananas = opt[Int]()
      codependent(apples, bananas)
      verify()
    }

    new Conf(Seq())
    intercept[ValidationFailure] {
      new Conf(Seq("-a", "1"))
    }
    intercept[ValidationFailure] {
      new Conf(Seq("-b", "1"))
    }
    new Conf(Seq("-a", "2", "-b", "1"))
  }

  test ("Arguments validation: custom validation") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      val apples = opt[Int]()
      val bananas = opt[Int]()
      validate (apples, bananas) { (a,b) =>
        if (a + b > 15) Right(())
        else Left("Not enough :(")
      }
      validateOpt (apples, bananas) {
        case (Some(a), None) => Left("I want to eat bananas with my apples!")
        case _ => Right(())
      }
      verify()
    }

    new Conf(Seq())
    intercept[ValidationFailure] {
      new Conf(Seq("-a", "1"))
    }
    intercept[ValidationFailure] {
      new Conf(Seq("-b", "1"))
    }
    expectException(ValidationFailure("Not enough :(")) {
      new Conf(Seq("-a", "1", "-b", "1"))
    }
    new Conf(Seq("-a", "14", "-b", "2"))
  }

  test ("Basic usage") {
    class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
      val apples = opt[Int](required = true)
      val bananas = opt[Int]()
      val name = trailArg[String]()
      verify()
    }

    val conf = new Conf(Seq("--apples", "4", "--bananas", "10", "bigBunny"))
    conf.apples() shouldBe 4
    conf.bananas.toOption shouldBe Some(10)
    conf.name() shouldBe "bigBunny"

    def someInternalFunc(conf: Conf): Unit = {
      conf.apples() shouldBe 4
    }
    someInternalFunc(conf)
  }

  test ("Custom converters") {
    case class Person(name: String, phone: String)

    val personConverter = new ValueConverter[Person] {
      val nameRgx = """([A-Za-z]*)""".r
      val phoneRgx = """([0-9\-]*)""".r
      // parse is a method, that takes a list of arguments to all option invokations:
      // for example, "-a 1 2 -a 3 4 5" would produce List(List(1,2),List(3,4,5)).
      // parse returns Left with error message, if there was an error while parsing
      // if no option was found, it returns Right(None)
      // and if option was found, it returns Right(...)
      def parse(s:List[(String, List[String])]):Either[String,Option[Person]] =
        s match {
          case (_, nameRgx(name) :: phoneRgx(phone) :: Nil) :: Nil =>
            Right(Some(Person(name,phone))) // successfully found our person
          case Nil => Right(None) // no person found
          case _ => Left("provide person name") // error when parsing
        }

      val argType = org.rogach.scallop.ArgType.LIST
    }

    object Conf extends ScallopConf(List("--person", "Pete", "123-45")) {
      val person = opt[Person]()(personConverter)
      verify()
    }
    Conf.person() shouldBe Person("Pete", "123-45")
  }

  test ("Help information printing: version summary") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      version("test 1.2.3 (c) 2012 Mr Placeholder")
      // ... options ...
      verify()
    }

    val (stdout, stderr, exits) = captureOutputAndExits {
      new Conf(Seq("--version"))
    }
    stdout shouldBe "test 1.2.3 (c) 2012 Mr Placeholder\n"
    stderr shouldBe ""
    exits shouldBe List(0)
  }

  test ("Help information printing: help page") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      version("test 1.2.3 (c) 2012 Mr Placeholder")
      banner("""Usage: test [OPTION]... [tree|palm] [OPTION]... [tree-name]
               |test is an awesome program, which does something funny
               |Options:
               |""".stripMargin)
      footer("\nFor all other tricks, consult the documentation!")

      val properties = props[String](descr = "some key-value pairs")
      val verbose = opt[Boolean](descr = "use more verbose output")
      val amount = opt[Int]("amount", descr = "how many objects do you need?")

      val tree = new Subcommand("tree") {
        val height = opt[Double](descr = "how tall should the tree be?")
        val name = trailArg[String]("tree name", descr = "tree name")
      }
      addSubcommand(tree)

      val palm = new Subcommand("palm") {
        val height = opt[Double](descr = "how tall should the palm be?")
        val name = trailArg[String]("tree name", descr = "palm name")
      }
      addSubcommand(palm)

      verify()
    }

    val expectedHelpText = """
test 1.2.3 (c) 2012 Mr Placeholder
Usage: test [OPTION]... [tree|palm] [OPTION]... [tree-name]
test is an awesome program, which does something funny
Options:

  -a, --amount  <arg>          how many objects do you need?
  -Dkey=value [key=value]...   some key-value pairs
  -v, --verbose                use more verbose output
  -h, --help                   Show help message
      --version                Show version of this program

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
""".stripPrefix("\n")

    val (stdout, stderr, exits) = captureOutputAndExits {
      new Conf(Seq("--help"))
    }

    stdout shouldBe expectedHelpText
    stderr shouldBe ""
    exits shouldBe List(0)
  }

  test ("Help information printing: subcommand help page") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      object push extends Subcommand("push") {
        banner("Update remote refs")
        val all = opt[Boolean]()
        val repo = opt[String]()
        footer("part of git suite")
      }
      addSubcommand(push)

      verify()
    }

    val expectedHelpText = """
Update remote refs
  -a, --all
  -r, --repo  <arg>
  -h, --help          Show help message
part of git suite
""".stripPrefix("\n")

    val (stdout, stderr, exits) = captureOutputAndExits {
      new Conf(Seq("push", "--help"))
    }

    stdout shouldBe expectedHelpText
    stderr shouldBe ""
    exits shouldBe List(0)
  }

  test ("Help information printing: option groups") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      val primaryFruits = group("Primary fruits:")
      val bananas    = opt[Int](descr = "amount of bananas", group = primaryFruits)
      val apples     = opt[Int](descr = "amount of apples", group = primaryFruits)

      val secondaryFruits = group(header = "Secondary fruits:")
      val coconuts   = opt[Int](descr = "amount of coconuts", group = secondaryFruits)
      val dewberries = opt[Int](descr = "amount of dewberries", group = secondaryFruits)

      val zucchini = opt[Int](descr = "amount of zucchini")
      verify()
    }

    val expectedHelpText = """
 Primary fruits:
  -b, --bananas  <arg>      amount of bananas
  -a, --apples  <arg>       amount of apples

 Secondary fruits:
  -c, --coconuts  <arg>     amount of coconuts
  -d, --dewberries  <arg>   amount of dewberries

  -z, --zucchini  <arg>     amount of zucchini
  -h, --help                Show help message
""".stripPrefix("\n")

    val (stdout, stderr, exits) = captureOutputAndExits {
      new Conf(Seq("--help"))
    }

    stdout shouldBe expectedHelpText
    stderr shouldBe ""
    exits shouldBe List(0)
  }

  test ("Help information printing: summary") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      val donkey = opt[Boolean]("donkey")
      val monkeys = opt[Int]("monkeys", default = Some(2))
      val numLimbs = opt[Int]("num-limbs")
      val params = opt[String]("params")
      val debug = opt[String]("debug")
      val properties = props[Int]('D')
      val petName = trailArg[String]("pet name")
      verify()
    }

    new Conf(Seq("-d", "--num-limbs", "1", "-Dalpha=1", "-D", "beta=2", "gamma=3", "Pigeon")).summary.shouldBe("""
Scallop(-d, --num-limbs, 1, -Dalpha=1, -D, beta=2, gamma=3, Pigeon)
 *  donkey => true
    monkeys => 2
 *  num-limbs => 1
    params => <None>
    debug => <None>
 *  D => Map(alpha -> 1, beta -> 2, gamma -> 3)
 *  pet name => Pigeon
""".stripPrefix("\n"))
  }

  test ("Option types and definitions: trailing arguments 1") {
    object Conf extends ScallopConf(List("aoeu", "asdf")) {
      val first = trailArg[String]()
      val second = trailArg[String](required = false)
      verify()
    }
    Conf.first() shouldBe "aoeu"
    Conf.second.toOption shouldBe Some("asdf")
  }

  test ("Option types and definitions: trailing arguments 2") {
    object Conf extends ScallopConf(
           List("-Ekey1=value1", "key2=value2", "key3=value3",
                "first", "1","2","3","second","4","5","6")) {
      val properties = props[String]('E')
      val firstListName = trailArg[String]()
      val firstList = trailArg[List[Int]]()
      val secondListName = trailArg[String]()
      val secondList = trailArg[List[Double]]()
      verify()
    }
    Conf.properties("key1") shouldBe "value1"
    Conf.firstListName() shouldBe "first"
    Conf.secondListName() shouldBe "second"
    Conf.firstList() shouldBe List(1,2,3)
    Conf.secondList() shouldBe List[Double](4,5,6)
  }

  test ("Option types and definitions: unnamed integer options 1") {
    object Conf extends ScallopConf(Seq("-42")) {
      val answer = number()
      verify()
    }
    Conf.answer() shouldBe 42
  }

  test ("Option types and definitions: unnamed integer options 2") {
    object Conf extends ScallopConf(Seq("-1", "-2", "-3")) {
      val first = number()
      val second = number()
      val third = number()
      verify()
    }
    Conf.first() shouldBe 1
    Conf.second() shouldBe 2
    Conf.third() shouldBe 3
  }

  test ("Option types and definitions: toggle options") {
    object Conf extends ScallopConf(List("--noverbose")) {
      val verbose = toggle("verbose")
      verify()
    }
    Conf.verbose.toOption shouldBe Some(false)
    Conf.verbose.isSupplied shouldBe true
  }

  test ("Option types and definitions: tally options") {
    object Conf extends ScallopConf(List("-vvv")) {
      val verbose = tally()
      verify()
    }
    Conf.verbose() shouldBe 3
  }

  test ("Option types and definitions: choice options") {
    object Conf extends ScallopConf(List("--fruit", "apple")) {
      val fruit = choice(Seq("apple", "banana", "coconut"))
      verify()
    }
    Conf.fruit() shouldBe "apple"
  }

  test ("Option types and definitions: verification") {
    intercept[IncompleteBuildException] {
      class Conf(args: Seq[String]) extends ScallopConf(args) {
        val apples = opt[Int]("apples")
        println(apples()) // <- don't do this
        val name = trailArg[String]()
        verify()
      }
      val conf = new Conf(Seq())
      conf.name() // to prevent "never used" error
    }
  }

  test ("Subcommands: basic example") {
    object Conf extends ScallopConf(Seq("-a", "tree", "-b")) {
      val apples = opt[Boolean]("apples")
      object tree extends Subcommand("tree") {
        val bananas = opt[Boolean]("bananas")
      }
      addSubcommand(tree)

      verify()
    }

    Conf.apples() shouldBe true
    Conf.subcommand shouldBe Some(Conf.tree)
    Conf.subcommands shouldBe List(Conf.tree)
    Conf.tree.bananas() shouldBe true
  }

  test ("Subcommands: nesting") {
    object Conf extends ScallopConf(Seq("sub1", "sub2", "sub3", "sub4", "win!")) {
      object sub1 extends Subcommand("sub1") {
        object sub2 extends Subcommand("sub2") {
          object sub3 extends Subcommand("sub3") {
            object sub4 extends Subcommand("sub4") {
              val opts = trailArg[List[String]]()
            }
            addSubcommand(sub4)
          }
          addSubcommand(sub3)
        }
        addSubcommand(sub2)
      }
      addSubcommand(sub1)
      verify()
    }

    Conf.subcommands shouldBe List(Conf.sub1, Conf.sub1.sub2, Conf.sub1.sub2.sub3, Conf.sub1.sub2.sub3.sub4)
    Conf.sub1.sub2.sub3.sub4.opts() shouldBe List("win!")
  }

  test ("Subcommands: sharing via inheritance") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      trait B { this: ScallopConf =>
        val o = opt[Boolean](name = "o")
      }
      object c1 extends Subcommand("c1") with B
      addSubcommand(c1)
      object c2 extends Subcommand("c2") with B
      addSubcommand(c2)
      verify()
    }

    new Conf(Seq("c1", "-o")).c1.o() shouldBe true
    new Conf(Seq("c2", "-o")).c2.o() shouldBe true
  }

}
