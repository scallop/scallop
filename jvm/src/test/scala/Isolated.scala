package org.rogach.scallop

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

/** Playground for new tests (to make better use of test-only) */
class Isolated extends UsefulMatchers with CapturingTest {
  throwError.value = true

  test ("i") {
    object Conf extends ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify()
    }
    Conf.apples() should equal (3)
  }

}
