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
                        |
                        |Subcommand: tree
                        |  -b, --branches  <arg>   how many branches? 
                        |
                        | trailing arguments:
                        |  trail (required)   Which trail do you choose? 
                        |Subcommand: peach
                        |plant the fruit-bearing peach tree
                        |  -p, --peaches  <arg>   how many peaches? 
                        |Latin name: Prunus persica
                        |
                        |Subcommand: palm
                        |  -l, --leaves  <arg>   how many leaves? 
                        |and some more
                        |""".stripMargin)
  }
  
  test ("help wrapping") {
    val opts = Scallop()
      .opt[Boolean]("apples", descr = "********* ********* ********* ********* ********* *********")
    opts.setHelpWidth(80).help should equal ("""  -a, --apples   ********* ********* ********* ********* ********* ********* """)
    opts.setHelpWidth(40).help should equal ("""  -a, --apples   ********* ********* 
                                               |                 ********* ********* 
                                               |                 ********* ********* """.stripMargin)
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
        |
        |Subcommands:
        |  tree        Plant a normal, regular tree
        |  peach       Plant a peach tree.
        |  submarine   
        |""".stripMargin
  }

}
