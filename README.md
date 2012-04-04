Scallop
========
A simple CLI parsing library for Scala, written in spirit of Ruby's [Trollop](http://trollop.rubyforge.org/). Works on Scala 2.9.x.

Scallop supports POSIX-style long (--opt) and short (-a, -abc) options, and property args (-Dkey=value, -D key1=value key2=value), 
extracting lists of argumets to option, and matching on trailing arguments.

It should be noted that the whole option builder is completely immutable (thus thread-safe), so you can reuse it, delegate
argument construction to sub-modules, etc. 

Installation
============

Add following to your build.sbt:

```scala
resolvers += "Rogach's maven repo" at "https://github.com/Rogach/org.rogach/raw/master/"

libraryDependencies += "org.rogach" %% "scallop" % "0.2.3"
```

Examples
========

```scala
import org.rogach.scallop._;

val opts = Scallop(List("-d","--num-limbs","1"))
  .version("test 1.2.3 (c) 2012 Mr S") // --version option is provided for you
                                       // in "verify" stage it would print this message and exit
  .banner("""Usage: test [OPTION]... [pet-name]
            |test is an awesome program, which does something funny      
            |Options:
            |""".stripMargin) // --help is also provided
                              //  will also exit after printing version, banner, and options usage
  .opt[Boolean]("donkey", descr = "use donkey mode") // simple flag option
  .opt("monkeys", default = Some(2), short = 'm') // you can add the default option
                                                  // the type will be inferred
  .opt[Int]("num-limbs", 'k', 
    "number of libms", required = true) // you can override the default short-option character
  .opt[List[Double]]("params") // default converters are provided for all primitives
                               //and for lists of primitives
  .props('D',"some key-value pairs")
  .args(List("-Dalpha=1","-D","betta=2","gamma=3", "Pigeon")) // you can add parameters a bit later
  .trailArg[String]("pet name") // you can specify what do you want to get from the end of 
                                // args list
  .verify
  
opts.get[Boolean]("donkey") should equal (Some(true))
opts[Int]("monkeys") should equal (2)
opts[Int]("num-limbs") should equal (1)
opts.prop('D',"alpha") should equal (Some("1"))
opts.prop('E',"gamma") should equal (None)
opts[String]("pet name") should equal ("Pigeon")
intercept[WrongTypeRequest] {
  opts[Double]("monkeys") // this will throw an exception at runtime
                          // because the wrong type is requested
}

println(opts.help) // returns options description
```

If you will run this option setup with "--help" option, you would see:

```
test 1.2.3 (c) 2012 Mr Placeholder
Usage: test [OPTION]...
test is an awesome program, which does something funny      
Options:

-Dkey=value [key=value]...
    some key-value pairs
-d, --donkey  
    use donkey mode
-m, --monkeys  <arg>
-k, --num-limbs  <arg>
    number of libms
-p, --params  <arg>...
```

Matching on the trailing arguments can get quite fancy thanks to Scallop's backtracking parser
- for example, it correctly handles the following case:

```scala
val opts = Scallop(List("-Ekey1=value1", "key2=value2", "key3=value3", 
                        "first", "1","2","3","second","4","5","6"))
  .props('E')
  .trailArg[String]("first list name")
  .trailArg[List[Int]]("first list values")
  .trailArg[String]("second list name")
  .trailArg[List[Double]]("second list values")
  .verify
opts.propMap('E') should equal ((1 to 3).map(i => ("key"+i,"value"+i)).toMap)
opts[String]("first list name") should equal ("first")
opts[String]("second list name") should equal ("second")
opts[List[Int]]("first list values") should equal (List(1,2,3))
opts[List[Double]]("second list values") should equal (List[Double](4,5,6))
```

Also, you can define your own custom converter for options:

```scala
case class Person(name:String, phone:String)
val personConverter = new ValueConverter[Person] {
  val nameRgx = """([A-Za-z]*)""".r
  val phoneRgx = """([0-9\-]*)""".r
  // parse is a method, that takes a list of arguments to all option invocations:
  // for example, "-a 1 2 -a 3 4 5" would produce List(List(1,2),List(3,4,5)).
  // parse returns Left, if there was an error while parsing
  // if no option was found, it returns Right(None)
  // and if option was found, it returns Right(...)
  def parse(s:List[List[String]]):Either[Unit,Option[Person]] = 
    s match {
      case ((nameRgx(name) :: phoneRgx(phone) :: Nil) :: Nil) => 
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
```

For more examples, you can consult Scallop's [test suite](https://github.com/Rogach/scallop/tree/master/src/test/scala)
or consult the [API docs](http://rogach.github.com/scallop/#org.rogach.scallop.package).

Thanks
------
* [Alexy Khrabrov](https://github.com/alexy)

... and the whole Scala community for help and explanations.