package org.rogach.scallop

class OptionsReaderTest extends ScallopTestBase {

  test ("reading options from stdin") {
    withInput("-a 3\n-b 5") {
      object Conf extends ScallopConf(List("@--")) {
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")

        verify()
      }
      Conf.apples() shouldBe 3
      Conf.bananas() shouldBe 5
    }
  }

  test ("reading options from file") {
    object Conf extends ScallopConf(List("@src/test/resources/opts.txt")) {
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")

      verify()
    }
    Conf.apples() shouldBe 3
    Conf.bananas() shouldBe 5
  }

}
