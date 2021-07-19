package org.rogach.scallop

class HelpTest extends ScallopTestBase {

  test ("help formatting - simple example") {
    object Conf extends ScallopConf(Seq()) {
      version("")
      val apples = opt[Int]("apples")
      verify()
    }
    Conf.getHelpString() shouldBe """  -a, --apples  <arg>
                                    |  -h, --help            Show help message
                                    |  -v, --version         Show version of this program""".stripMargin
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
          addSubcommand(tree)

          verify()
        }
        Conf
      }
      exits.size should equal (1)
    }
    out should equal ("""0.1.2
                        |  -a, --apples  <arg>   fresh apples!
                        |  -h, --help            Show help message
                        |  -v, --version         Show version of this program
                        |
                        |Subcommand: tree - some tree
                        |  -b, --branches  <arg>   how many branches?
                        |  -h, --help              Show help message
                        |""".stripMargin)
  }

  test ("help wrapping") {
    class Conf(newHelpWidth: Int) extends ScallopConf(Seq()) {
      version("")
      helpWidth(newHelpWidth)
      val apples = opt[Boolean]("apples", descr = "********* ********* ********* ********* ********* *********")
      verify()
    }

    new Conf(80).getHelpString() shouldBe """  -a, --apples    ********* ********* ********* ********* ********* *********
                                            |  -h, --help      Show help message
                                            |  -v, --version   Show version of this program""".stripMargin
    new Conf(40).getHelpString() shouldBe """  -a, --apples    ********* *********
                                            |                  ********* *********
                                            |                  ********* *********
                                            |  -h, --help      Show help message
                                            |  -v, --version   Show version of this
                                            |                  program""".stripMargin
  }

  test ("help descr with '\\n'") {
    class Conf(newHelpWidth: Int, appendDefaults: Boolean) extends ScallopConf(Seq()) {
      version("")
      helpWidth(newHelpWidth)
      this.appendDefaultToDescription = appendDefaults
      val arg1 = opt[String](descr = "********* ********* ********* ********* *********", default = Some("arg1"))
      val arg2 = opt[String](descr = "********* ********* ********* ********* *********\n********* ********* ********* ********* *********", default = Some("arg2"))
      val arg3 = opt[String](default = Some("arg3"))
      val arg4 = opt[String]()
      verify()
    }

    new Conf(100, true).getHelpString() shouldBe {
      """  -a, --arg1  <arg>   ********* ********* ********* ********* ********* (default = arg1)
        |      --arg2  <arg>   ********* ********* ********* ********* *********
        |                      ********* ********* ********* ********* *********
        |                      (default = arg2)
        |      --arg3  <arg>   (default = arg3)
        |      --arg4  <arg>
        |  -h, --help          Show help message
        |  -v, --version       Show version of this program""".stripMargin
    }
    new Conf(50, true).getHelpString() shouldBe {
      """  -a, --arg1  <arg>   ********* *********
        |                      ********* *********
        |                      ********* (default = arg1)
        |      --arg2  <arg>   ********* *********
        |                      ********* *********
        |                      *********
        |                      ********* *********
        |                      ********* *********
        |                      *********
        |                      (default = arg2)
        |      --arg3  <arg>   (default = arg3)
        |      --arg4  <arg>
        |  -h, --help          Show help message
        |  -v, --version       Show version of this program""".stripMargin
    }

    new Conf(100, false).getHelpString() shouldBe {
      """  -a, --arg1  <arg>   ********* ********* ********* ********* *********
        |      --arg2  <arg>   ********* ********* ********* ********* *********
        |                      ********* ********* ********* ********* *********
        |      --arg3  <arg>
        |      --arg4  <arg>
        |  -h, --help          Show help message
        |  -v, --version       Show version of this program""".stripMargin
    }
    new Conf(50, false).getHelpString() shouldBe {
      """  -a, --arg1  <arg>   ********* *********
        |                      ********* *********
        |                      *********
        |      --arg2  <arg>   ********* *********
        |                      ********* *********
        |                      *********
        |                      ********* *********
        |                      ********* *********
        |                      *********
        |      --arg3  <arg>
        |      --arg4  <arg>
        |  -h, --help          Show help message
        |  -v, --version       Show version of this program""".stripMargin
    }
  }

  test ("version printing") {
    val (out, err) = captureOutput {
      val exits = trapExit {
        object Conf extends ScallopConf(List("--version")) {
          version("0.1.2")
          val apples = opt[Int]("apples")
          verify()
        }
        Conf
      }
      exits.size should equal (1)
    }
    out should equal ("""0.1.2
                        |""".stripMargin)
  }

  test ("version printing (short argument)") {
    val (out, err) = captureOutput {
      val exits = trapExit {
        object Conf extends ScallopConf(List("-v")) {
          version("0.1.2")
          val apples = opt[Int]("apples")
          verify()
        }
        Conf
      }
      exits.size should equal (1)
    }
    out should equal ("""0.1.2
                        |""".stripMargin)
  }

  test ("help printing (short argument)") {
    captureOutput {
      val exits = trapExit {
        object Conf extends ScallopConf(List("-h")) {
          val apples = opt[Int]("apples")
          verify()
        }
        Conf
      }
      exits.size should equal (1)
    }
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
        addSubcommand(tree)

        verify()
      }
      Conf
    }

    exits shouldBe List(0)
    err shouldBe ""
    out shouldBe
     """Planting a tree
       |  -a, --apples  <arg>   how many apples?
       |  -h, --help            Show help message
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
          addSubcommand(peach)
        }
        addSubcommand(tree)

        verify()
      }
      Conf
    }

    exits shouldBe List(0)
    err shouldBe ""
    out shouldBe
     """  -a, --apples  <arg>   how many apples?
       |  -h, --help            Show help message
       |""".stripMargin

  }

  test ("short format for subcommands help") {
    val (out, _, _) = captureOutputAndExits {
      object Conf extends ScallopConf(Seq("--help")) {
        banner("Planting a tree.")
        shortSubcommandsHelp()
        val bananas = opt[Int]("bananas")

        val tree = new Subcommand("tree") {
          descr("Plant a normal, regular tree")
          val apples = opt[Int]("apples")
        }
        addSubcommand(tree)

        val peach = new Subcommand("peach") {
          descr("Plant a peach tree.")
        }
        addSubcommand(peach)

        val submarine = new Subcommand("submarine") {}
        addSubcommand(submarine)

        verify()
      }
      Conf
    }

    out shouldBe
      """Planting a tree.
        |  -b, --bananas  <arg>
        |  -h, --help             Show help message
        |
        |Subcommands:
        |  tree        Plant a normal, regular tree
        |  peach       Plant a peach tree.
        |  submarine
        |""".stripMargin
  }

  test ("user-provided help & version option takes precedence over hardcoded one") {
    object Conf extends ScallopConf(Nil) {
      val help = opt[Boolean](noshort = true, descr = "custom help descr")
      val version = opt[Boolean](noshort = true, descr = "custom version descr")

      verify()
    }
    Conf.builder.help shouldBe
      """      --help      custom help descr
        |      --version   custom version descr""".stripMargin
  }

  test ("user-provided help option works with short-named argument") {
    val (out, err) = captureOutput {
      val exits = trapExit {
        object Conf extends ScallopConf(Seq("-?")) {
          val help = opt[Boolean]("help", short = '?', descr = "custom help descr")

          verify()
        }
        Conf
      }
      exits.size shouldBe 1
    }
    out shouldBe
      """  -?, --help   custom help descr
        |""".stripMargin
  }

  test ("nested subcommands are reflected in help") {
    val (out, err) = captureOutput {
      val exits = trapExit {
        object Conf extends ScallopConf(List("--help")) {
          val tree = new Subcommand("tree") {
            val branches = opt[Int]("branches", descr = "how many branches?")
            val trail = trailArg[String]("trail", descr = "Which trail do you choose?")

            val peach = new Subcommand("peach") {
              banner("plant the fruit-bearing peach tree")
              val peaches = opt[Int]("peaches", descr = "how many peaches?")

              val palm = new Subcommand("palm") {
                val leaves = opt[Int]("leaves", descr = "how many leaves?")
              }
              addSubcommand(palm)

              footer("Latin name: Prunus persica\n")
            }
            addSubcommand(peach)
          }
          addSubcommand(tree)

          verify()
        }
        Conf
      }
      exits.size should equal (1)
    }
    out should equal ("""  -h, --help   Show help message
                        |
                        |Subcommand: tree
                        |  -b, --branches  <arg>   how many branches?
                        |  -h, --help              Show help message
                        |
                        | trailing arguments:
                        |  trail (required)   Which trail do you choose?
                        |
                        |Subcommand: tree peach
                        |plant the fruit-bearing peach tree
                        |  -p, --peaches  <arg>   how many peaches?
                        |  -h, --help             Show help message
                        |
                        |Subcommand: tree peach palm
                        |  -l, --leaves  <arg>   how many leaves?
                        |  -h, --help            Show help message
                        |Latin name: Prunus persica
                        |""".stripMargin)
  }

  test ("append default values to help output if requested") {
    object Conf extends ScallopConf(Nil) {
      appendDefaultToDescription = true
      val apples = opt[Int](descr = "amount of apples", default = Some(42))
      verify()
    }
    Conf.builder.help shouldBe
      """  -a, --apples  <arg>   amount of apples (default = 42)
        |  -h, --help            Show help message""".stripMargin
  }

  test ("append default values to help output of subcommand if parent has this setting") {
    val (out, _, _) = captureOutputAndExits {
      new ScallopConf(Seq("--help")) {
        appendDefaultToDescription = true
        val tree = new Subcommand("tree") {
          val apples = opt[Int](descr = "how many apples?", default = Some(42))
        }
        addSubcommand(tree)

        verify()
      }
    }

    out shouldBe
     """  -h, --help   Show help message
       |
       |Subcommand: tree
       |  -a, --apples  <arg>   how many apples? (default = 42)
       |  -h, --help            Show help message
       |""".stripMargin

  }

  test ("handle default value printing in subcommand help") {
    val (out, _, _) = captureOutputAndExits {
      new ScallopConf(Seq("tree", "--help")) {
        appendDefaultToDescription = true
        val tree = new Subcommand("tree") {
          val apples = opt[Int](descr = "how many apples?", default = Some(42))
        }
        addSubcommand(tree)

        verify()
      }
    }

    out shouldBe
     """  -a, --apples  <arg>   how many apples? (default = 42)
       |  -h, --help            Show help message
       |""".stripMargin

  }

  test ("hide help option if overriding option has hidden = true") {
    val conf = new ScallopConf(Seq()) {
      val help = opt[Boolean](hidden = true)
      verify()
    }
    val (out, _, _) = captureOutputAndExits {
      conf.printHelp()
    }
    out shouldBe
      """
        |""".stripMargin
  }

  test ("hide help option in subcommand if overriding option has hidden = true") {
    val conf = new ScallopConf(Seq()) {
      val sub = new Subcommand("sub") {
        val help = opt[Boolean](hidden = true)
      }
      addSubcommand(sub)
      verify()
    }
    val (out, _, _) = captureOutputAndExits {
      conf.printHelp()
    }
    out shouldBe
      """  -h, --help   Show help message
        |
        |Subcommand: sub
        |""".stripMargin
  }

  test ("custom help formatter ('Subcommand' => 'Command')") {
    val conf = new ScallopConf(Seq()) {
      val tree = new Subcommand("tree") {
        val apples = opt[Int](descr = "how many apples?")
      }
      addSubcommand(tree)

      helpFormatter = new ScallopHelpFormatter {
        override def getSubcommandHeaderPrefix = "Command: "
      }

      verify()
    }
    conf.builder.help shouldBe
      """  -h, --help   Show help message
        |
        |Command: tree
        |  -a, --apples  <arg>   how many apples?
        |  -h, --help            Show help message""".stripMargin
  }

  test ("custom help formatter ('trailing arguments' => 'arguments')") {
    val conf = new ScallopConf(Seq("42")) {
      val apples = trailArg[Int](descr = "how many apples?")

      helpFormatter = new ScallopHelpFormatter {
        override def getTrailingArgsSectionName = " arguments:"
      }

      verify()
    }
    conf.builder.help shouldBe
      """  -h, --help   Show help message
        |
        | arguments:
        |  apples (required)   how many apples?""".stripMargin
  }

  test ("implicit short option name should override help printing") {
    val exits = trapExit {
      object Conf extends ScallopConf(Seq("-h")) {
        val hard = opt[Boolean]()
        verify()
      }
      Conf.hard() shouldBe true
    }
    exits.size shouldBe 0
  }

  test ("implicit short option name should override version printing") {
    val exits = trapExit {
      object Conf extends ScallopConf(Seq("-v")) {
        val verbose = opt[Boolean]()
        version("1.0")
        verify()
      }
      Conf.verbose() shouldBe true
    }
    exits.size shouldBe 0
  }

  test ("implicit short option name in subcommand should override help printing") {
    val exits = trapExit {
      object Config extends ScallopConf(Seq("cmd", "-h")) {
        object cmd extends Subcommand("cmd") {
          val hard = opt[Boolean]()
        }
        addSubcommand(cmd)
        verify()
      }
      Config.cmd.hard() shouldBe true
    }
    exits.size shouldBe 0
  }

  test ("implicit short option name in subcommand should override version printing") {
    val exits = trapExit {
      object Config extends ScallopConf(Seq("cmd", "-v")) {
        version("Version 1.0")
        object cmd extends Subcommand("cmd") {
          val verbose = opt[Boolean]()
        }
        addSubcommand(cmd)
        verify()
      }
      Config.cmd.verbose() shouldBe true
    }
    exits.size shouldBe 0
  }

  test ("help formatter wrapping test") {
    val text = Formatter.wrap("supress all output, including output from scripts (stderr from scripts is still printed)".split(" ").toSeq, 76)
    val expected = (List(
      "supress all output, including output from scripts (stderr from scripts is ",
      "still printed) "
    ))
    text shouldBe expected
  }

}
