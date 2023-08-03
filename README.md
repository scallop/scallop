Scallop
========

[![Build Status](https://img.shields.io/github/workflow/status/scallop/scallop/Continuous%20Integration)](https://github.com/scallop/scallop/actions?query=workflow%3A%22Continuous+Integration%22)

A simple command-line arguments parsing library for Scala.
Cross-built for Scala 3.0, 2.13, 2.12, 2.11, 2.10, supports Scala Native and Scala JS.

Scallop supports:

* flag, single-value and multiple value options
* POSIX-style short option names (-a) with grouping (-abc)
* GNU-style long option names (--opt, --opt=value)
* unnamed integer options, like GNU tail (-42)
* Property arguments (-Dkey=value, -D key1=value key2=value)
* Non-string types of options and properties values (with extendable converters)
* Powerful matching on trailing args
* Subcommands

For more info and information on usage, you can look into the [project wiki](https://github.com/scallop/scallop/wiki) or consult the [API docs](http://scallop.github.io/scallop/org/rogach/scallop/index.html).

Also, I wrote a [blog post](http://blog.rogach.org/2012/04/better-cli-option-parsing-in-scala.html) and [another one](http://blog.rogach.org/2012/04/configuration-objects-in-scallop.html) about Scallop.

Installation
============

Add following to your build.sbt:

```scala
libraryDependencies += "org.rogach" %% "scallop" % "5.0.0"
```

For use with Scala Native and Scala.js, use `%%%`:

```scala
libraryDependencies += "org.rogach" %%% "scallop" % "5.0.0"
```

If you were using `4.x` version or older, please see [migration notes](https://github.com/scallop/scallop/wiki/Migration-notes).

Quick example
=============

```scala
import org.rogach.scallop._

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val apples = opt[Int](required = true)
  val bananas = opt[Int]()
  val name = trailArg[String]()
  verify()
}

object Main {
  def main(args: Array[String]) {
    val conf = new Conf(args)
    println("apples are: " + conf.apples())
  }
}
```

This snippet above defined simple configuration that will parse argument lines like these:

```
--apples 4 --bananas 10 strangeTree
-a 4 appleTree
```

For more examples, you can look at Scallop's [wiki](https://github.com/scallop/scallop/wiki) and [test suite](./jvm/src/test/scala).

Fancy things
============

Scallop supports quite powerful matching on trailing arguments. For example:

```scala
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
```

In this case, Scallop's backtracking parser is clever enough to distinguish the boundaries of the arguments lists.

Also, Scallop supports parsing of subcommands. Not only subcommands, but nested subcommands!

```scala
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
```

Thanks
======
* [Alexy Khrabrov](https://github.com/alexy)

... and the whole Scala community for help and explanations.

Notes
=====

Scallop is distributed under [MIT license](./license.txt).
