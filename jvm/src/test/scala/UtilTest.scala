package org.rogach.scallop

class UtilTest extends ScallopTestBase {

  test ("format") {
    Util.format("%s", "test") shouldBe "test"
    Util.format(" %s ", "test") shouldBe " test "
    Util.format(" %d ", 42) shouldBe " 42 "
    Util.format("%s%s", "AB", "CD") shouldBe "ABCD"
    Util.format("%s %s", "AB", "CD") shouldBe "AB CD"
    Util.format("%s %s%s", "AB", "CD", "EF") shouldBe "AB CDEF"
    Util.format("%1$s %1$s", "A") shouldBe "A A"
    Util.format("%1$s %2$s %1$s", "A", "B") shouldBe "A B A"
  }

}
