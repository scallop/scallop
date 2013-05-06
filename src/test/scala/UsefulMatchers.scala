package org.rogach.scallop

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

trait UsefulMatchers extends FunSuite with ShouldMatchers {

  implicit def toGoodEquals[A](a: A) = new {
    def ====[B](b: B) = a should equal (b)
  }
}
