Scrollop
========

A simple CLI parsing library for Scala, written in spirit of Ruby's [Trollop](http://trollop.rubyforge.org/). Works on Scala 2.9.x.

It supports POSIX-style long (--opt) and short (-a, -abc) options.

Installation
============

Add following to your build.sbt:

```scala
resolvers += "Rogach's maven repo" at "https://github.com/Rogach/org.rogach/raw/master/"

libraryDependencies += "org.rogach" % "scrollop" % "0.1.6"
```

Examples
========

```scala
import org.rogach.scrollop._;
val opts = Scrollop(args)
  .version("test 1.2.3 (c) 2012 Mr S") // --version option is provided for you
                                       // in "verify" stage it would print this message and exit
  .banner("""Usage: test [OPTION]...
            |test is an awesome program, which does something funny      
            |Options:
            |""".stripMargin) // --help is also provided
                              //  will also exit after printing version, banner, and options usage
  .opt[Boolean]("donkey", descr = "use donkey mode") // simple flag option
  .opt("monkey", descr = "monkey mode", default = Some(true)) // you can add the default option
                                                              // the type will be inferred
  .opt[Int]("num-limbs", 'k',
      "number of libms", required = true) // you can override the default short-option character
  .opt[List[Double]]("params") // default converters are provided for all primitives
                               // and for lists of primitives
  .opt[Double]("alpha", arg = "value") // you can change the name of the argument in "help" output
  .verify

// option retreiving
opts[Boolean]("donkey")
opts[Double]("monkey") // this will throw an exception at runtime, since the wrong type is requested
opts.get[List[Double]]("params") // Option[List[Double]]
```

If you will run this with "--help" option, you would see:

```
test 1.2.3 (c) 2012 Mr Placeholder
Usage: test [OPTION]...
test is an awesome program, which does something funny      
Options:

-a, --alpha  <value>
-d, --donkey  
    use donkey mode
-m, --monkey  
    monkey mode
-k, --num-limbs  <arg>
    number of libms
-p, --params  <arg>...
```