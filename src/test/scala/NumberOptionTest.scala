package org.rogach.scallop

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class NumberOptionTest extends FunSuite with Matchers with CapturingTest with UsefulMatchers {
  throwError.value = true

  test ("number option") {
    val conf = new ScallopConf(Seq("-42")) {
      val answer = number()
      verify()
    }
    conf.answer() shouldBe 42
  }

  test ("required number option provided") {
    val conf = new ScallopConf(Seq("-42")) {
      val answer = number(required = true)
      verify()
    }
    conf.answer.get shouldBe Some(42)
  }

  test ("required number option not provided") {
    expectException(RequiredOptionNotFound("answer")) {
      val conf = new ScallopConf(Nil) {
        val answer = number(required = true)
        verify()
      }
    }
  }

  test ("multiple number options") {
    val conf = new ScallopConf(Seq("-1", "-2", "-3")) {
      val first = number()
      val second = number()
      val third = number()
      verify()
    }
    conf.first() shouldBe 1
    conf.second() shouldBe 2
    conf.third() shouldBe 3
  }

  test ("number option validation success") {
    val conf = new ScallopConf(Seq("-42")) {
      val answer = number(validate = _ > 10)
      verify()
    }
  }

  test ("number option validation failure") {
    expectException(ValidationFailure("Validation failure for 'answer' option parameters: 42")) {
      val conf = new ScallopConf(Seq("-42")) {
        val answer = number(validate = _ < 10)
        verify()
      }
    }
  }

  test ("number option help text") {
    val (out, err) = captureOutput {
      val conf = new ScallopConf() {
        val first = number("a", descr = "option a")
        val second = number("b", descr = "option b")
        verify()
      }
      conf.printHelp()
    }
    out shouldBe """  -<a>         option a
  -<b>         option b
      --help   Show help message
"""
    err shouldBe ""
  }

}
