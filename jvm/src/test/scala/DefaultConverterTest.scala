package org.rogach.scallop

import org.rogach.scallop.exceptions._
import scala.concurrent.duration._

class DurationConverterTest extends ScallopTestBase {
  test ("convert to Duration") {
    case class getcf(args0: Seq[String]) extends ScallopConf(args0) {
      val foo = opt[Duration]()
      verify()
    }

    getcf(List("-f", "1 minute")).foo.toOption shouldBe Some(1.minute)
    getcf(List("-f", "Inf")).foo.toOption shouldBe Some(Duration.Inf)
    getcf(List("-f", "MinusInf")).foo.toOption shouldBe Some(Duration.MinusInf)

    expectException(WrongOptionFormat("foo", "bar", "java.lang.NumberFormatException: format error bar")) {
      getcf(List("-f", "bar")).foo.toOption shouldBe Some(Duration.MinusInf)
    }
  }
}

class FiniteDurationConverterTest extends ScallopTestBase {
  test ("convert to Duration") {
    case class getcf(args0: Seq[String]) extends ScallopConf(args0) {
      val foo = opt[FiniteDuration]()
      verify()
    }

    getcf(List("-f", "1 minute")).foo.toOption shouldBe Some(1.minute)

    expectException(WrongOptionFormat("foo", "Inf", "java.lang.IllegalArgumentException: 'Duration.Inf' is not a FiniteDuration.")) {
      getcf(List("-f", "Inf")).foo.toOption shouldBe Some(Duration.Inf)
    }

    expectException(WrongOptionFormat("foo", "bar", "java.lang.NumberFormatException: format error bar")) {
      getcf(List("-f", "bar")).foo.toOption shouldBe Some(Duration.MinusInf)
    }
  }
}
