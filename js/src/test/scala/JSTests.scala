package org.rogach.scallop

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class JSTests extends AnyFunSuite with Matchers {

  test("small example") {
    val conf = new ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify()
    }
    conf.apples() shouldBe 3
    conf.builder.help shouldBe """  -a, --apples  <arg>
                                 |  -h, --help            Show help message""".stripMargin
  }

}
