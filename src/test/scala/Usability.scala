package org.rogach.scallop

import exceptions._

class Usability extends UsefulMatchers {
  throwError.value = true

  test ("printing error message for single argument") {
    expectException(WrongOptionFormat("apples", "asdf", "wrong arguments format")) {
      new ScallopConf(Seq("-a","asdf")) {
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")

        verify()
      }
    }
  }

}
