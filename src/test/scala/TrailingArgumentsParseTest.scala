package org.rogach.scallop

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class TrailingArgumentsParseTest extends FunSuite with Matchers with UsefulMatchers {
  throwError.value = true

  test ("non-required trailing option after flag") {
    val conf = new ScallopConf(Seq("-a")) {
      val apple = opt[Boolean]("apple")
      val banana = trailArg[String](required = false)

      verify()
    }
    conf.apple.get shouldBe Some(true)
    conf.banana.get shouldBe None
  }

  test ("proper error message on trailing file option failure") {
    expectException(WrongOptionFormat("file", "nonexistent", "file 'nonexistent' doesn't exist")) {
      new ScallopConf(Seq("nonexistent")) {
        val file = trailArg[java.io.File]("file")

        verify()
      }
    }
  }
}
