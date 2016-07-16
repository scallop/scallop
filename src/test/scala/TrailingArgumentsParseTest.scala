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
    conf.apple.toOption shouldBe Some(true)
    conf.banana.toOption shouldBe None
  }

  test ("proper error message on trailing file option failure") {
    expectException(ValidationFailure("File 'nonexistent' not found")) {
      new ScallopConf(Seq("nonexistent")) {
        val file = trailArg[java.io.File]("file")
        validateFileExists(file)

        verify()
      }
    }
  }

  test ("proper error message on trailing path option failure") {
    expectException(ValidationFailure("File at 'nonexistent' not found")) {
      new ScallopConf(Seq("nonexistent")) {
        val path = trailArg[java.nio.file.Path]("path")
        validatePathExists(path)

        verify()
      }
    }
  }
}
