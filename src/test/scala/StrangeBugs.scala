package org.rogach.scallop

import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class StrangeBugs extends UsefulMatchers {
  throwError.value = true
  
  test ("funny bug with option name guessing") {
    object Conf extends ScallopConf(Seq("-b")) {
      val nonGuessedName = opt[Boolean]("bananas")
    }
    Conf

    intercept[IdenticalOptionNames] {
      val conf = new ScallopConf() {
        guessOptionName = true
        val guessedName = opt[Int]()
        val nonGuessedName = opt[Int]()
      }
    }
  }
  
}
