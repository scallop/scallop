package org.rogach.scallop

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.rogach.scallop.exceptions._

class ToggleOptionTest extends AnyFunSuite with Matchers {
  throwError.value = true

  test ("short name") {
    object Conf extends ScallopConf(Seq("-e")) {
      val answer = toggle(name = "tgl-option", short = 'e', default = Some(false))
      verify()
    }
    Conf.answer() shouldBe true
  }

  test ("unknown option") {
    assertThrows[UnknownOption] {
      object Conf extends ScallopConf(Seq("-t")) {
        val answer = toggle(name = "tgl-option", short = 'e', default = Some(false))
        verify()
      }
      Conf
    }
  }


}
