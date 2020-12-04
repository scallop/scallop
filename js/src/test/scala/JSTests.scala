package org.rogach.scallop

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class JSTests extends AnyFunSuite with Matchers {

  test("small example") {
    object Conf extends ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify()
    }
    Conf.apples() shouldBe 3
    Conf.builder.help shouldBe """  -a, --apples  <arg>
                                 |  -h, --help            Show help message""".stripMargin
  }

}
