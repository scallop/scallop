package org.rogach.scallop

import org.rogach.scallop.exceptions._

class StrangeTest extends UsefulMatchers with CapturingTest {
  throwError.value = false

  test ("reading options from stdin") {
    withInput("-a 3\n-b 5") {
      object Conf extends ScallopConf(List("@--")) {
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
      }
      Conf.apples() should equal (3)
      Conf.bananas() should equal (5)
    }
  }

  test ("reading options from file") {
    object Conf extends ScallopConf(List("@src/test/resources/opts.txt")) {
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
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
