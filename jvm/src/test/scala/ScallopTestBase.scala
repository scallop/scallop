package org.rogach.scallop

import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

trait ScallopTestBase extends AnyFunSuite with Matchers with CapturingTest with BeforeAndAfter {

  protected def getThrowErrorValue: Boolean = true

  before {
    throwError.value = getThrowErrorValue
  }

  after {
    throwError.value = false
  }

  def expectException(ex: Throwable)(fn: => Any): Unit = {
    try {
      fn
    } catch {
      case e: Throwable =>
        e shouldBe ex
        return
    }
    assert(false, "expected exception " + ex + ", none thrown")
  }

}
