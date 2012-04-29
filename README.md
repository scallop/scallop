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

libraryDependencies += "org.rogach" %% "scallop" % "0.3.7"
```

Quick example
=============

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

For more info, you can look into the [project wiki](https://github.com/Rogach/scallop/wiki) or consult the [API docs](http://rogach.github.com/scallop/#org.rogach.scallop.package).

For more examples, you can look at Scallop's [test suite](https://github.com/Rogach/scallop/tree/master/src/test/scala).

Also, I wrote a [blog post](http://rogach-scala.blogspot.com/2012/04/better-cli-option-parsing-in-scala.html) and [another one](http://rogach-scala.blogspot.com/2012/04/configuration-objects-in-scallop.html) about Scallop.

Fancy things
============

Scallop supports quite powerful matching on trailing arguments. For example:

```scala
object Conf extends ScallopConf(
       List("-Ekey1=value1", "key2=value2", "key3=value3", 
            "first", "1","2","3","second","4","5","6")) {
  val props = props[String]('E')
  val firstListName = trailArg[String]()
  val firstList = trailArg[List[Int]]()
  val secondListName = trailArg[String]()
  val secondList = trailArg[List[Double]]()
  verify
}
Conf.props("key1") should equal (Some("value1"))
Conf.firstListName() should equal ("first")
Conf.secondListName() should equal ("second")
Conf.firstList() should equal (List(1,2,3))
Conf.secondList() should equal (List[Double](4,5,6))
```

In this case, Scallops backtracking parser is clever enough to distinguish the boundaries of the arguments lists.

Thanks
======
* [Alexy Khrabrov](https://github.com/alexy)

... and the whole Scala community for help and explanations.