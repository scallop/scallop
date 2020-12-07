package org.rogach.scallop

/** Playground for new tests (to make better use of test-only) */
class Isolated extends UsefulMatchers with CapturingTest {
  throwError.value = true

  test ("i") {
    object Conf extends ScallopConf(Seq()) {
      verify()
    }
    Conf.args shouldBe Seq()
  }

}
