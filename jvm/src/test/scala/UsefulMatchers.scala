package org.rogach.scallop

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

trait UsefulMatchers extends AnyFunSuite with Matchers {

  implicit def toGoodEquals[A](a: A) = new {
    def ====[B](b: B) = a should equal (b)
  }

  def expectException(ex: Throwable)(fn: => Any): Unit = {
    try {
      fn
    } catch {
      case e: Throwable =>
        e ==== ex
        return
    }
    assert(false, "expected exception " + ex + ", none thrown")
  }
}
