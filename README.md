Scallop
========
A simple command-line arguments parsing library for Scala, written in spirit of Ruby's [Trollop](http://trollop.rubyforge.org/). Works on Scala 2.9.x.

Scallop supports:

* flag, single-value and multiple value options
* POSIX-style short option names (-a) with grouping (-abc)
* GNU-style long option names (--opt)
* Property arguments (-Dkey=value, -D key1=value key2=value)
* Non-string types of options and properties values (with extendable converters)
* Powerful matching on trailing args

It should be noted that the whole option builder is completely immutable (thus thread-safe), so you can reuse it, delegate
argument construction to sub-modules, etc. 

Installation
============

Add following to your build.sbt:

```scala
resolvers += "Rogach's maven repo" at "https://github.com/Rogach/org.rogach/raw/master/"

libraryDependencies += "org.rogach" %% "scallop" % "0.3.4"
```

Examples
========

Using the Conf class
--------------------

Advantages of using ScallopConf include complete type-safety (thus less explicit types) and ability to pass the resulting object into other functions.

```scala
import org.rogach.scallop._;

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
```

Using the builder
-----------------

Using the builder is more flexible choice, since you can pass the builder around and even add arguments after you defined all options. On the other side, type-safety is a bit relaxed - if you request the wrong type, there is no way to protect you at compile-time (but there is indeed some protection in run-time).

```scala
import org.rogach.scallop._;

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
  .opt("monkeys", default = Some(2), short = 'm') // you can add the default option
                                                  // the type will be inferred
  .opt[Int]("num-limbs", 'k', 
    "number of libms", required = true) // you can override the default short-option character
  .opt[List[Double]]("params") // default converters are provided for all primitives
                               //and for lists of primitives
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
opts.prop[String]('E',"gamma") should equal (None)
opts[String]("pet name") should equal ("Pigeon")
intercept[WrongTypeRequest] {
  opts[Double]("monkeys") // this will throw an exception at runtime
                          // because the wrong type is requested
}

println(opts.help) // returns options description

println(opts.summary) // returns summary of parser status (with current arg values)
```

If you will run this option setup with "--help" option, you would see:

```
test 1.2.3 (c) 2012 Mr Placeholder
Usage: test [OPTION]... [pet-name]
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

For all other tricks, consult the documentation!
```

Misc
----

The following examples can be used with both ScallopConf and with builder.

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

For more examples, you can look at Scallop's [test suite](https://github.com/Rogach/scallop/tree/master/src/test/scala)
or consult the [API docs](http://rogach.github.com/scallop/#org.rogach.scallop.package).

Also, I wrote a [blog post](http://rogach-scala.blogspot.com/2012/04/better-cli-option-parsing-in-scala.html) and [another one](http://rogach-scala.blogspot.com/2012/04/configuration-objects-in-scallop.html) about Scallop.

Thanks
------
* [Alexy Khrabrov](https://github.com/alexy)

... and the whole Scala community for help and explanations.