package org.rogach.scallop

class HelpTest extends UsefulMatchers with CapturingTest {
  throwError.value = false

  test ("help printing") {
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
          val peach = new Subcommand("peach") {
            banner("plant the fruit-bearing peach tree")
            val peaches = opt[Int]("peaches", descr = "how many peaches?")
            footer("Latin name: Prunus persica\n")
          }
          val palm = new Subcommand("palm") {
            val leaves = opt[Int]("leaves", descr = "how many leaves?")
          }
          verify
        }
        Conf
      }
      exits.size should equal (1)
    }
    out should equal ("""0.1.2
                        |some rubbish
                        |  -a, --apples  <arg>   fresh apples! (default = 3)
                        |  -v, --verbose         very verbose
                        |      --noverbose       turn off
                        |      --help            Show help message
                        |      --version         Show version of this program
                        |
                        |Subcommand: tree
                        |  -b, --branches  <arg>   how many branches?
                        |      --help              Show help message
                        |
                        | trailing arguments:
                        |  trail (required)   Which trail do you choose?
                        |Subcommand: peach
                        |plant the fruit-bearing peach tree
                        |  -p, --peaches  <arg>   how many peaches?
                        |      --help             Show help message
                        |Latin name: Prunus persica
                        |
                        |Subcommand: palm
                        |  -l, --leaves  <arg>   how many leaves?
                        |      --help            Show help message
                        |and some more
                        |""".stripMargin)
  }

  test ("subcommand description in output") {
    val (out, err) = captureOutput {
      val exits = trapExit {
        object Conf extends ScallopConf(List("--help")) {
          version("0.1.2")
          val apples = opt[Int]("apples", descr = "fresh apples!")
          val tree = new Subcommand("tree") {
            descr("some tree")
            val branches = opt[Int]("branches", descr = "how many branches?")
          }
          verify
        }
        Conf
      }
      exits.size should equal (1)
    }
    out should equal ("""0.1.2
                        |  -a, --apples  <arg>   fresh apples!
                        |      --help            Show help message
                        |      --version         Show version of this program
                        |
                        |Subcommand: tree - some tree
                        |  -b, --branches  <arg>   how many branches?
                        |      --help              Show help message
                        |""".stripMargin)
  }

  test ("help wrapping") {
    val opts = Scallop()
      .version("")
      .opt[Boolean]("apples", descr = "********* ********* ********* ********* ********* *********")
    opts.setHelpWidth(80).help should equal ("""  -a, --apples    ********* ********* ********* ********* ********* *********
                                               |      --help      Show help message
                                               |      --version   Show version of this program""".stripMargin)
    opts.setHelpWidth(40).help should equal ("""  -a, --apples    ********* *********
                                               |                  ********* *********
                                               |                  ********* *********
                                               |      --help      Show help message
                                               |      --version   Show version of this
                                               |                  program""".stripMargin)
  }

  test ("version printing") {
    val (out, err) = captureOutput {
      val exits = trapExit {
        object Conf extends ScallopConf(List("--version")) {
          version("0.1.2")
          val apples = opt[Int]("apples")
          verify
        }
        Conf
      }
      exits.size should equal (1)
    }
    out should equal ("""0.1.2
                        |""".stripMargin)
  }

  test ("help for subcommand") {
    val (out, err, exits) = captureOutputAndExits {
      object Conf extends ScallopConf(Seq("tree", "--help")) {
        val tree = new Subcommand("tree") {
          banner("Planting a tree")
          val apples = opt[Int](descr = "how many apples?")
          val jang = trailArg[String]("jang")
          footer("finish planting.")
        }
      }
      Conf
    }
    exits ==== List(0)
    err ==== ""
    out ====
     """Planting a tree
       |  -a, --apples  <arg>   how many apples?
       |      --help            Show help message
       |
       | trailing arguments:
       |  jang (required)
       |finish planting.
       |""".stripMargin
  }

  test ("help for subcommand two levels deep") {
    val (out, err, exits) = captureOutputAndExits {
      object Conf extends ScallopConf(Seq("tree", "peach", "--help")) {
        val tree = new Subcommand("tree") {
          val peach = new Subcommand("peach") {
            val apples = opt[Int](descr = "how many apples?")
          }
        }
      }
      Conf
    }
    exits ==== List(0)
    err ==== ""
    out ====
     """  -a, --apples  <arg>   how many apples?
       |      --help            Show help message
       |""".stripMargin

  }

  test ("short format for subcommands help") {
    val (out, _, _) = captureOutputAndExits {
      object Conf extends ScallopConf(Seq("--help")) {
        banner("Planting a tree.")
        shortSubcommandsHelp()
        val bananas = opt[Int]()
        val tree = new Subcommand("tree") {
          descr("Plant a normal, regular tree")
          val apples = opt[Int]()
        }
        val peach = new Subcommand("peach") {
          descr("Plant a peach tree.")
        }
        val submarine = new Subcommand("submarine") {()}
      }
      Conf
    }
    out ====
      """Planting a tree.
        |  -b, --bananas  <arg>
        |      --help             Show help message
        |
        |Subcommands:
        |  tree        Plant a normal, regular tree
        |  peach       Plant a peach tree.
        |  submarine
        |""".stripMargin
  }

  test ("splitting commands list into 'main' and 'other' options") {
    object Conf extends ScallopConf(Nil) {
      mainOptions = Seq(bananas, apples)
      val apples     = opt[Int](descr = "amount of apples")
      val bananas    = opt[Int](descr = "amount of bananas")
      val coconuts   = opt[Int](descr = "amount of coconuts")
      val dewberries = opt[Int](descr = "amount of dewberries")
    }
    Conf.builder.help ====
      """  -b, --bananas  <arg>      amount of bananas
        |  -a, --apples  <arg>       amount of apples
        |
        |  -c, --coconuts  <arg>     amount of coconuts
        |  -d, --dewberries  <arg>   amount of dewberries
        |      --help                Show help message""".stripMargin
  }

  test ("splitting commands list into 'main' and 'other' options (in subcommands)") {
    object Conf extends ScallopConf(Nil) {
      val plant = new Subcommand("plant") {
        mainOptions = Seq(bananas, apples)
        val apples     = opt[Int](descr = "amount of apples")
        val bananas    = opt[Int](descr = "amount of bananas")
        val coconuts   = opt[Int](descr = "amount of coconuts")
        val dewberries = opt[Int](descr = "amount of dewberries")
      }
    }
    Conf.plant.builder.help ====
      """  -b, --bananas  <arg>      amount of bananas
        |  -a, --apples  <arg>       amount of apples
        |
        |  -c, --coconuts  <arg>     amount of coconuts
        |  -d, --dewberries  <arg>   amount of dewberries
        |      --help                Show help message""".stripMargin
  }

  test ("user-provided help & version option takes precedence over hardcoded one") {
    object Conf extends ScallopConf(Nil) {
      val help = opt[Boolean](noshort = true, descr = "custom help descr")
      val version = opt[Boolean](noshort = true, descr = "custom version descr")
    }
    Conf.builder.help ====
      """      --help      custom help descr
        |      --version   custom version descr""".stripMargin
  }

}
