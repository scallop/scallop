package org.rogach.scallop

/** Playground for new tests (to make better use of test-only) */
class Isolated extends ScallopTestBase {

  test ("i") {
    throwError.withValue(true) {
      object Conf extends ScallopConf(Seq()) {
        val apples = opt[Int]()
        verify()
      }
      Conf.args shouldBe Seq()
    }
  }

}
