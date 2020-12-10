package org.rogach.scallop

/*
  to test only this suite:

  sbt 'testOnly org.rogach.scallop.ComplexTests'

 */
class ComplexTests extends ScallopTestBase {

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
      Conf.builder.printHelp()
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

  test ("help printing - complex example") {
    val (out, err) = captureOutput {
      val exits = trapExit {
        object Conf extends ScallopConf(List("--help")) {
          version("0.1.2")
          banner("some rubbish")
          footer("and some more")
          val apples = opt[Int]("apples", descr = "fresh apples!", default = Some(3))
          val verbose = toggle("verbose", descrYes = "very verbose", descrNo = "turn off")

          val tree = new Subcommand("tree") {
            val branches = opt[Int]("branches", descr = "how many branches?")
            val trail = trailArg[String]("trail", descr = "Which trail do you choose?")
          }
          addSubcommand(tree)

          val peach = new Subcommand("peach") {
            banner("plant the fruit-bearing peach tree")
            val peaches = opt[Int]("peaches", descr = "how many peaches?")
            footer("Latin name: Prunus persica\n")
          }
          addSubcommand(peach)

          val palm = new Subcommand("palm", "palm-tree") {
            val leaves = opt[Int]("leaves", descr = "how many leaves?")
            val hiddenTrail = trailArg[String]("secret", descr = "secret trailing arg", hidden = true)
          }
          addSubcommand(palm)

          verify()
        }
        Conf
      }
      exits.size shouldBe 1
    }
    out should equal ("""0.1.2
                        |some rubbish
                        |  -a, --apples  <arg>   fresh apples!
                        |  -v, --verbose         very verbose
                        |      --noverbose       turn off
                        |  -h, --help            Show help message
                        |      --version         Show version of this program
                        |
                        |Subcommand: tree
                        |  -b, --branches  <arg>   how many branches?
                        |  -h, --help              Show help message
                        |
                        | trailing arguments:
                        |  trail (required)   Which trail do you choose?
                        |Subcommand: peach
                        |plant the fruit-bearing peach tree
                        |  -p, --peaches  <arg>   how many peaches?
                        |  -h, --help             Show help message
                        |Latin name: Prunus persica
                        |
                        |Subcommand: palm (alias: palm-tree)
                        |  -l, --leaves  <arg>   how many leaves?
                        |  -h, --help            Show help message
                        |and some more
                        |""".stripMargin)
  }

}
