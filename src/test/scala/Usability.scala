package org.rogach.scallop

import exceptions._

class Usability extends UsefulMatchers {
  throwError.value = true

  test ("printing error message for single argument") {
    var s = false
    try {
      val conf = new ScallopConf(Seq("-a","asdf")) {
        val apples = opt[Int]()
        val bananas = opt[Int]()
      }
    } catch {
      case WrongOptionFormat("apples", "asdf") => s = true
      case _ =>
    }
    s ==== true
  }
}
