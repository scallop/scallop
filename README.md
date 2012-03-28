Scallop
========

A simple CLI parsing library for Scala, written in spirit of Ruby's [Trollop](http://trollop.rubyforge.org/). Works on Scala 2.9.x.

It supports POSIX-style long (--opt) and short (-a, -abc) options, and property args (-Dkey=value, -D key1=value key2=value).

Installation
============

Add following to your build.sbt:

```scala
resolvers += "Rogach's maven repo" at "https://github.com/Rogach/org.rogach/raw/master/"

libraryDependencies += "org.rogach" %% "scallop" % "0.2.0"
```

Examples
========

```scala
import org.rogach.scallop._;

val opts = Scallop(List("-d","--num-limbs","1"))
  .version("test 1.2.3 (c) 2012 Mr S") // --version option is provided for you
                                       // in "verify" stage it would print this message and exit
  .banner("""Usage: test [OPTION]...
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
  .args(List("-Dalpha=1","-D","betta=2","gamma=3")) // you can add parameters a bit later
  .verify
  
opts.get[Boolean]("donkey") should equal (Some(true))
opts[Int]("monkeys") should equal (2)
opts[Int]("num-limbs") should equal (1)
opts.prop('D',"alpha") should equal (Some("1"))
opts.prop('E',"gamma") should equal (None)
intercept[WrongTypeRequest] {
  opts[Double]("monkeys") // this will throw an exception at runtime
                          // because the wrong type is requested
}
```

If you will run this with "--help" option, you would see:

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