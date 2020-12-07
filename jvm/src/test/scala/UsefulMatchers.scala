package org.rogach.scallop

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

trait UsefulMatchers extends AnyFunSuite with Matchers {
  abstract class GoodEquals[A](val a: A) {
    def ====[B](b: B): Assertion
  }

  implicit def toGoodEquals[A](a0: A): GoodEquals[A] = new GoodEquals[A](a0) {
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
