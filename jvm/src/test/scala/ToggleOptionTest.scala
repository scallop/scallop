package org.rogach.scallop

import org.scalatest.{FunSuite, Matchers}
import org.rogach.scallop._
import org.rogach.scallop.exceptions._


class ToggleOptionTest extends FunSuite with Matchers {
  throwError.value = true

  test ("short name") {
    val conf = new ScallopConf(Seq("-e")) {
      val answer = toggle(name = "tgl-option", short = 'e', default = Some(false))
      verify()
    }
    conf.answer() shouldBe true
  }

  test("unknown option") {
    assertThrows[UnknownOption] {
      val conf = new ScallopConf(Seq("-t")) {
        val answer = toggle(name = "tgl-option", short = 'e', default = Some(false))
        verify()
      }
    }
  }


}
