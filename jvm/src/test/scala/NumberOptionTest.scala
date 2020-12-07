package org.rogach.scallop

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.rogach.scallop.exceptions._

class NumberOptionTest extends AnyFunSuite with Matchers with CapturingTest with UsefulMatchers {
  throwError.value = true

  test ("number option") {
    object Conf extends ScallopConf(Seq("-42")) {
      val answer = number()
      verify()
    }
    Conf.answer() shouldBe 42
  }

  test ("required number option provided") {
    object Conf extends ScallopConf(Seq("-42")) {
      val answer = number(required = true)
      verify()
    }
    Conf.answer.toOption shouldBe Some(42)
  }

  test ("required number option not provided") {
    expectException(RequiredOptionNotFound("answer")) {
      object Conf extends ScallopConf(Nil) {
        val answer = number(required = true)
        verify()
      }
      Conf
    }
  }

  test ("multiple number options") {
    object Conf extends ScallopConf(Seq("-1", "-2", "-3")) {
      val first = number()
      val second = number()
      val third = number()
      verify()
    }
    Conf.first() shouldBe 1
    Conf.second() shouldBe 2
    Conf.third() shouldBe 3
  }

  test ("number option validation success") {
    object Conf extends ScallopConf(Seq("-42")) {
      val answer = number(validate = _ > 10)
      verify()
    }
    Conf.answer() shouldBe 42
  }

  test ("number option validation failure") {
    expectException(ValidationFailure("Validation failure for 'answer' option parameters: 42")) {
      object Conf extends ScallopConf(Seq("-42")) {
        val answer = number(validate = _ < 10)
        verify()
      }
      Conf
    }
  }

  test ("number option help text") {
    val (out, err) = captureOutput {
      object Conf extends ScallopConf() {
        val first = number("a", descr = "option a")
        val second = number("b", descr = "option b")
        verify()
      }
      Conf.printHelp()
    }
    out shouldBe """  -<a>         option a
  -<b>         option b
  -h, --help   Show help message
"""
    err shouldBe ""
  }

}
