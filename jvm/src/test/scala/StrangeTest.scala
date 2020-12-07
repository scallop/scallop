package org.rogach.scallop

class StrangeTest extends UsefulMatchers with CapturingTest {
  throwError.value = false

  test ("reading options from stdin") {
    withInput("-a 3\n-b 5") {
      object Conf extends ScallopConf(List("@--")) {
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")

        verify()
      }
      Conf.apples() should equal (3)
      Conf.bananas() should equal (5)
    }
  }

  test ("reading options from file") {
    object Conf extends ScallopConf(List("@src/test/resources/opts.txt")) {
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")

      verify()
    }
    Conf.apples() should equal (3)
    Conf.bananas() should equal (5)
  }

  test ("changing printed program name") {
    overrideColorOutput.withValue(Some(false)) {
      val (out, err, exits) = captureOutputAndExits {
        object Conf extends ScallopConf(Seq()) {
          val apples = trailArg[Int]("beans")
          printedName = "beans"

          verify()
        }
        Conf
      }
      exits ==== List(1)
      out ==== ""
      err ==== "[beans] Error: Required option 'beans' not found\n"
    }
  }

}
