import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class NormalTest extends FunSuite with ShouldMatchers {

  test ("main") {
val opts = Scallop(List("-d","--num-limbs","1"))
  .version("test 1.2.3 (c) 2012 Mr Placeholder") // --version option is provided for you
                                       // in "verify" stage it would print this message and exit
  .banner("""Usage: test [OPTION]... [pet-name]
            |test is an awesome program, which does something funny
            |Options:
            |""".stripMargin) // --help is provided, will also exit after printing version,
                              // banner, options usage, and footer
  .footer("\nFor all other tricks, consult the documentation!")
  .opt[Boolean]("donkey", descr = "use donkey mode") // simple flag option
  .opt("monkeys", default = () => Some(2), short = 'm') // you can add the default option
                                                  // the type will be inferred
  .opt[Int]("num-limbs", 'k',
    "number of libms", required = true) // you can override the default short-option character
  .opt[List[Double]]("params") // default converters are provided for all primitives
                               // and for lists of primitives
  .opt[String]("debug", hidden = true) // hidden parameters are not printed in help
  .props[String]('D',"some key-value pairs") // yes, property args can have types on their values too
  .args(List("-Dalpha=1","-D","betta=2","gamma=3", "Pigeon")) // you can add parameters a bit later
  .trailArg[String]("pet name") // you can specify what do you want to get from the end of
                                // args list
  .verify

opts.get[Boolean]("donkey") should equal (Some(true))
opts[Int]("monkeys") should equal (2)
opts[Int]("num-limbs") should equal (1)
opts.prop[String]('D',"alpha") should equal (Some("1"))
opts[String]("pet name") should equal ("Pigeon")
intercept[WrongTypeRequest] {
  opts[Double]("monkeys") // this will throw an exception at runtime
                          // because the wrong type is requested
}

opts.printHelp
println
println(opts.summary) // returns summary of parser status (with current arg values)

//opts.args(List("--help")).verify
  }

  test ("no values") {
    Scallop().verify
    Scallop(Array[String]()).verify
    Scallop(List()).verify
  }

  test ("long flag") {
    val opts = Scallop(List("--angel"))
      .opt[Boolean]("angel")
      .verify
    opts[Boolean]("angel") should equal (true)
  }

  test ("short flag, explicit name") {
    val opts = Scallop(List("-a"))
      .opt[Boolean]("angel", short = 'a')
      .verify
    opts[Boolean]("angel") should equal (true)
  }

  test ("short flag, implicit name") {
    val opts = Scallop(List("-a"))
      .opt[Boolean]("angel")
      .verify
    opts[Boolean]("angel") should equal (true)
  }

  test ("two short flags, implicit name") {
    val opts = Scallop(List("-a"))
      .opt[Boolean]("angel")
      .opt[Boolean]("baboon")
      .verify
    opts[Boolean]("angel") should equal (true)
    opts[Boolean]("baboon") should equal (false)
  }

  test ("two short flags, implicit name, required value") {
    val opts = Scallop(List("-a"))
      .opt[Boolean]("angel", required = true)
      .opt[Boolean]("baboon")
      .verify
    opts[Boolean]("angel") should equal (true)
    opts[Boolean]("baboon") should equal (false)
  }

  test ("one missing int, short opt") {
    val opts = Scallop(List())
      .opt[Int]("angels")
      .verify
    opts.get[Int]("angels") should equal (None)
  }

  test ("one int, short opt") {
    val opts = Scallop(List("-a","42"))
      .opt[Int]("angels")
      .verify
    opts.get[Int]("angels") should equal (Some(42))
  }

  test ("one int, long opt") {
    val opts = Scallop(List("--angels","42"))
      .opt[Int]("angels")
      .verify
    opts.get[Int]("angels") should equal (Some(42))
  }

  test ("one short, long opt") {
    val opts = Scallop(List("--angels","42"))
      .opt[Short]("angels")
      .verify
    opts.get[Short]("angels") should equal (Some(42))
  }

  test ("one byte, long opt") {
    val opts = Scallop(List("--angels","42"))
      .opt[Byte]("angels")
      .verify
    opts.get[Byte]("angels") should equal (Some(42))
  }

  test ("one double, long opt") {
    val opts = Scallop(List("--angels","42"))
      .opt[Double]("angels")
      .verify
    opts.get[Double]("angels") should equal (Some(42))
  }

  test ("one string, long opt") {
    val opts = Scallop(List("--angels","aoeu"))
      .opt[String]("angels")(stringConverter)
      .verify
    opts.get[String]("angels") should equal (Some("aoeu"))
  }


  test ("list of ints, long opt") {
    val opts = Scallop(List("--angels","42","12","345"))
      .opt[List[Int]]("angels")
      .verify
    opts.get[List[Int]]("angels") should equal (Some(List(42,12,345)))
  }

  test ("list of doubles, long opt") {
    val opts = Scallop(List("--angels","42.0","12","345e0"))
      .opt[List[Double]]("angels")
      .verify
    opts.get[List[Double]]("angels") should equal (Some(List(42.0,12.0,345.0)))
  }

  test ("default value") {
    val opts = Scallop(List())
      .opt("ang", default = () => Some(42), required = true)
      .verify
    opts[Int]("ang") should equal (42)
  }

  test ("additional args") {
    val opts = Scallop(List("-a","5"))
      .opt[List[Int]]("ang")
      .args(List("-a","10"))
      .verify
    opts[List[Int]]("ang") should equal (List(5,10))
  }

  // properties testing

  test ("no value") {
    val opts = Scallop()
      .props[String]('D')
      .verify
    opts.prop[String]('D',"aoeu") should equal (None)
  }

  test ("simle value") {
    val opts = Scallop(List("-Daoeu=htns"))
      .props[String]('D')
      .verify
    opts.prop[String]('D',"aoeu") should equal (Some("htns"))
  }

  test ("one plain prop") {
    val opts = Scallop(List("-D","aoeu=htns"))
      .props[String]('D')
      .verify
    opts.prop[String]('D',"aoeu") should equal (Some("htns"))
  }

  test ("two plain props") {
    val opts = Scallop(List("-D", "aoeu=htns", "qjk=gcr"))
      .props[String]('D')
      .verify
    opts.prop[String]('D',"aoeu") should equal (Some("htns"))
    opts.prop[String]('D',"qjk") should equal (Some("gcr"))
  }

  test ("two plain props to a map") {
    val opts = Scallop(List("-D", "aoeu=htns", "qjk=gcr"))
      .props[String]('D')
      .verify
    opts[Map[String,String]]('D') should equal (Map("aoeu" -> "htns", "qjk" -> "gcr"))
  }

  test ("empty prop") {
    val opts = Scallop(Nil)
      .props[String]('E')
      .verify
    opts.prop[String]('E',"nokey") should equal (None)
  }

  // --- typed props ---

  test ("typed int prop") {
    val opts = Scallop(List("-Ekey1=1","key2=2"))
      .props[Int]('E')
      .verify
    opts.prop[Int]('E',"key1") should equal (Some(1))
    opts.prop[Int]('E',"key2") should equal (Some(2))
    opts[Map[String,Int]]('E') should equal (Map("key1" -> 1, "key2" -> 2))
  }

  test ("typed double prop") {
    val opts = Scallop(List("-Ekey1=1.1","key2=2.3"))
      .props[Double]('E')
      .verify
    opts.prop[Double]('E',"key1") should equal (Some(1.1))
    opts.prop[Double]('E',"key2") should equal (Some(2.3))
  }

  test ("opt implicit name clash with prop name") {
    val opts = Scallop(List("-D", "aoeu=htn"))
      .props[String]('D')
      .opt[String]("Dark")
      .verify
    opts.get[String]("Dark") should equal (None)
  }

  test ("trail options - after single long-named argument") {
    val opts = Scallop(List("--echo", "42", "rabbit"))
      .opt[Int]("echo")
      .trailArg[String]("animal")
      .verify
    opts.get[Int]("echo") should equal ((Some(42)))
    opts[String]("animal") should equal ("rabbit")
  }

  test ("trail options - after single short-named argument") {
    val opts = Scallop(List("-e", "42", "rabbit"))
      .opt[Int]("echo")
      .trailArg[String]("animal")
      .verify
    opts.get[Int]("echo") should equal ((Some(42)))
    opts[String]("animal") should equal ("rabbit")
  }

  test ("trail options - after two arguments") {
    val opts = Scallop(List("-d","--num-limbs","1","Pigeon"))
      .opt[Boolean]("donkey", descr = "use donkey mode") // simple flag option
      .opt[Int]("num-limbs", 'k',
        "number of libms", required = true) // you can override the default short-option character
      .trailArg[String]("pet-name")
      .verify
    opts[Boolean]("donkey") should equal (true)
    opts.get[Int]("num-limbs") should equal ((Some(1)))
    opts[String]("pet-name") should equal ("Pigeon")
  }

  test ("trail options - after single property argument") {
    val opts = Scallop(List("-E", "key=value", "rabbit"))
      .props[String]('E')
      .trailArg[String]("animal")
      .verify
    opts.prop[String]('E',"key") should equal ((Some("value")))
    opts[String]("animal") should equal ("rabbit")
  }

  test ("trail options - after single property argument (2)") {
    val opts = Scallop(List("-E", "key=value", "rabbit", "key2=value2"))
      .props[String]('E')
      .trailArg[List[String]]("rubbish")
      .verify
    opts.prop[String]('E',"key") should equal ((Some("value")))
    opts[List[String]]("rubbish") should equal (List("rabbit", "key2=value2"))
  }

  test ("trail options - after list argument") {
    val opts = Scallop(List("--echo","42","43"))
      .opt[List[Int]]("echo")
      .trailArg[List[Int]]("numbers")
      .verify
    opts.get[List[Int]]("echo") should equal (Some(List(42)))
    opts.get[List[Int]]("numbers") should equal (Some(List(43)))
  }

  test ("trail options - after list argument, optional") {
    val opts = Scallop(List("--echo","42","43"))
      .opt[List[Int]]("echo")
      .trailArg[List[Int]]("numbers", required = false)
      .verify
    opts.get[List[Int]]("echo") should equal (Some(List(42,43)))
    opts.get[List[Int]]("numbers") should equal (None)
  }

  test ("trail options - after list argument, single optional value") {
    val opts = Scallop(List("--echo","42","43"))
      .opt[List[Int]]("echo")
      .trailArg[Int]("numbers", required = false)
      .verify
    opts.get[List[Int]]("echo") should equal (Some(List(42,43)))
    opts.get[Int]("numbers") should equal (None)
  }

  test ("trail options - after flag argument, single optional value") {
    val opts = Scallop(List("--echo","42","43"))
      .opt[Boolean]("echo")
      .trailArg[List[Int]]("numbers", required = false)
      .verify
    opts.get[List[Int]]("numbers") should equal (Some(List(42,43)))
    opts[Boolean]("echo") should equal (true)
  }

  test ("trail options - one required, one optional - both provided") {
    val opts = Scallop(List("first","second"))
      .trailArg[String]("name")
      .trailArg[String]("surname", required = false)
      .verify
    opts[String]("name") should equal ("first")
    opts.get[String]("surname") should equal (Some("second"))
  }

  test ("trail options - one required, one optional - one provided") {
    val opts = Scallop(List("first"))
      .trailArg[String]("name")
      .trailArg[String]("surname", required = false)
      .verify
    opts[String]("name") should equal ("first")
    opts.get[String]("surname") should equal (None)
  }

  test ("trail option - with default value, provided") {
    val opts = Scallop(List("first"))
      .trailArg[String]("name", required = false, default = () => Some("aoeu"))
      .verify
    opts[String]("name") should equal ("first")
  }

  test ("trail option - with default value, not provided") {
    val opts = Scallop(Nil)
      .trailArg[String]("name", required = false, default = () => Some("aoeu"))
      .verify
    opts[String]("name") should equal ("aoeu")
  }

  test ("trail options - tricky case") {
    val opts = Scallop(List("-Ekey1=value1", "key2=value2", "key3=value3",
                            "first", "1","2","3","second","4","5","6"))
      .props[String]('E')
      .trailArg[String]("first list name")
      .trailArg[List[Int]]("first list values")
      .trailArg[String]("second list name")
      .trailArg[List[Double]]("second list values")
      .verify
    opts[Map[String,String]]('E') should equal ((1 to 3).map(i => ("key"+i,"value"+i)).toMap)
    opts[String]("first list name") should equal ("first")
    opts[String]("second list name") should equal ("second")
    opts[List[Int]]("first list values") should equal (List(1,2,3))
    opts[List[Double]]("second list values") should equal (List[Double](4,5,6))
  }

  test ("custom converter example") {
    case class Person(name:String, phone:String)
    val personConverter = new ValueConverter[Person] {
      val nameRgx = """([A-Za-z]*)""".r
      val phoneRgx = """([0-9\-]*)""".r
      // parse is a method, that takes a list of arguments to all option invokations:
      // for example, "-a 1 2 -a 3 4 5" would produce List(List(1,2),List(3,4,5)).
      // parse returns Left, if there was an error while parsing
      // if no option was found, it returns Right(None)
      // and if option was found, it returns Right(...)
      def parse(s:List[(String, List[String])]):Either[Unit,Option[Person]] =
        s match {
          case (_, nameRgx(name) :: phoneRgx(phone) :: Nil) :: Nil =>
            Right(Some(Person(name,phone))) // successfully found our person
          case Nil => Right(None) // no person found
          case _ => Left(Unit) // error when parsing
        }
      val manifest = implicitly[Manifest[Person]] // some magic to make typing work
      val argType = org.rogach.scallop.ArgType.LIST
    }
    val opts = Scallop(List("--person", "Pete", "123-45"))
      .opt[Person]("person")(personConverter)
      .verify
    opts[Person]("person") should equal (Person("Pete", "123-45"))
  }

  test ("is supplied - option value was supplied") {
    val opts = Scallop(List("-a", "1"))
      .opt[Int]("apples")
      .verify
    opts.isSupplied("apples") should equal (true)
  }

  test ("is supplied - option value was not supplied, no default") {
    val opts = Scallop(Nil)
      .opt[Int]("apples")
      .verify
    opts.isSupplied("apples") should equal (false)
  }

  test ("is supplied - option value was not supplied, with default") {
    val opts = Scallop(Nil)
      .opt[Int]("apples", default = () => Some(7))
      .verify
    opts.isSupplied("apples") should equal (false)
  }

  test ("is supplied - trail arg value was supplied") {
    val opts = Scallop(List("first"))
      .trailArg[String]("file", required = false)
      .verify
    opts.isSupplied("file") should equal (true)
  }

  test ("is supplied - trail arg value was not supplied, no default value") {
    val opts = Scallop(Nil)
      .trailArg[String]("file", required = false)
      .verify
    opts.isSupplied("file") should equal (false)
  }

  test ("is supplied - trail arg value was not supplied, with default value") {
    val opts = Scallop(Nil)
      .trailArg[String]("file", default = () => Some("second"), required = false)
      .verify
    opts.isSupplied("file") should equal (false)
  }

  test ("noshort") {
    val opts = Scallop(List("-b","1"))
      .opt[Int]("bananas", noshort = true)
      .opt[Int]("bags")
      .verify
    opts.get[Int]("bananas") should equal (None)
    opts.get[Int]("bags") should equal (Some(1))
  }

  test ("negative number in option parameters") {
    val opts = Scallop(List("-a","-1"))
      .opt[Int]("apples")
      .verify
    opts.get[Int]("apples") should equal (Some(-1))
  }

  test ("correct validation") {
    val opts = Scallop(List("-a","1"))
      .opt[Int]("apples", validate = (0<))
      .verify
    opts[Int]("apples") should equal (1)
  }

  test ("help printing") {
    val opts = Scallop(List("--help"))
      .version("")
      .opt[Int]("apples")
    opts.help should equal ("""  -a, --apples  <arg>
                              |      --help            Show help message
                              |      --version         Show version of this program""".stripMargin)
  }

}
