package org.rogach.scallop

import org.rogach.scallop.exceptions._

import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.duration._

class DurationConverterTest extends AnyFunSuite with UsefulMatchers {
  throwError.value = true

  test("convert to Duration") {
    case class getcf(args0: Seq[String]) extends ScallopConf(args0) {
      val foo = opt[Duration]()
      verify()
    }

    getcf(List("-f", "1 minute")).foo.toOption ==== Some(1.minute)
    getcf(List("-f", "Inf")).foo.toOption ==== Some(Duration.Inf)
    getcf(List("-f", "MinusInf")).foo.toOption ==== Some(Duration.MinusInf)

    expectException(WrongOptionFormat("foo", "bar", "wrong arguments format")) {
      getcf(List("-f", "bar")).foo.toOption ==== Some(Duration.MinusInf)
    }
  }
}

class FiniteDurationConverterTest extends AnyFunSuite with UsefulMatchers {
  throwError.value = true

  test("convert to Duration") {
    case class getcf(args0: Seq[String]) extends ScallopConf(args0) {
      val foo = opt[FiniteDuration]()
      verify()
    }

    getcf(List("-f", "1 minute")).foo.toOption ==== Some(1.minute)

    expectException(WrongOptionFormat("foo", "Inf", "wrong arguments format")) {
      getcf(List("-f", "Inf")).foo.toOption ==== Some(Duration.Inf)
    }

    expectException(WrongOptionFormat("foo", "bar", "wrong arguments format")) {
      getcf(List("-f", "bar")).foo.toOption ==== Some(Duration.MinusInf)
    }
  }
}
