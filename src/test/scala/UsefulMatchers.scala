package org.rogach.scallop

import org.scalatest.FunSuite
import org.scalatest.Matchers

trait UsefulMatchers extends FunSuite with Matchers {

  implicit def toGoodEquals[A](a: A) = new {
    def ====[B](b: B) = a should equal (b)
  }

  def expectException(ex: Throwable)(fn: => Any) {
    try {
      fn
    } catch {
      case e: Throwable => e ==== ex
    }
  }
}
