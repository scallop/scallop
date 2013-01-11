package org.rogach.scallop

import org.rogach.scallop.exceptions._

class StrangeTest extends UsefulMatchers with CapturingTest {
  throwError.value = false
  
  test("help printing") {
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
                        |
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
  
  test ("reading options from stdin") {
    withInput("-a 3\n-b 5") {
      object Conf extends ScallopConf(List("@--")) {
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        verify
      }
      Conf.apples() should equal (3)
      Conf.bananas() should equal (5)
    }
  }
  
  test ("reading options from file") {
    object Conf extends ScallopConf(List("@src/test/resources/opts.txt")) {
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      verify
    }
    Conf.apples() should equal (3)
    Conf.bananas() should equal (5)
  }
  
  test ("changing printed program name") {
    val (out, err, exits) = captureOutputAndExits {
      object Conf extends ScallopConf(Seq()) {
        val apples = trailArg[Int]("beans")
        printedName = "beans"
      }
      Conf
    }
    exits ==== List(1)
    err ==== ""
    out ==== "[\033[31mbeans\033[0m] Error: Required option 'beans' not found\n"
  }
  
}
