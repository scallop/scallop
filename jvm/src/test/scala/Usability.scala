package org.rogach.scallop

import exceptions._

class Usability extends ScallopTestBase {

  test ("printing error message for single argument") {
    expectException(WrongOptionFormat("apples", "asdf", "bad Int value")) {
      object Conf extends ScallopConf(Seq("-a","asdf")) {
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")

        verify()
      }
      Conf
    }
  }

}
