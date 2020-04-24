package org.rogach.scallop

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class UtilTest extends AnyFunSuite with Matchers with UsefulMatchers {
  test ("format") {
    Util.format("%s", "test") ==== "test"
    Util.format(" %s ", "test") ==== " test "
    Util.format(" %d ", 42) ==== " 42 "
    Util.format("%s%s", "AB", "CD") ==== "ABCD"
    Util.format("%s %s", "AB", "CD") ==== "AB CD"
    Util.format("%s %s%s", "AB", "CD", "EF") ==== "AB CDEF"
    Util.format("%1$s %1$s", "A") ==== "A A"
    Util.format("%1$s %2$s %1$s", "A", "B") ==== "A B A"
  }
}
