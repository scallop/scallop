package org.rogach.scallop

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.rogach.scallop.exceptions._

class TrailingArgumentsParseTest extends AnyFunSuite with Matchers with UsefulMatchers {
  throwError.value = true

  test ("single one-arg trailing arg") {
    object conf extends ScallopConf(Seq("alpha")) {
      val trailing = trailArg[String]()
      verify()
    }
    conf.trailing() shouldBe "alpha"
  }

  test ("two one-arg trailing args") {
    object conf extends ScallopConf(Seq("alpha", "beta")) {
      val trailingAlpha = trailArg[String]()
      val trailingBeta = trailArg[String]()
      verify()
    }
    conf.trailingAlpha() shouldBe "alpha"
    conf.trailingBeta() shouldBe "beta"
  }

  test ("single multi-arg trailing arg") {
    object conf extends ScallopConf(Seq("alpha", "beta")) {
      val trailing = trailArg[List[String]]()
      verify()
    }
    conf.trailing() shouldBe List("alpha", "beta")
  }

  test ("two trailing args: one single arg, one multi-arg") {
    object conf extends ScallopConf(Seq("alpha", "beta", "gamma")) {
      val trailingSingle = trailArg[String]()
      val trailingMulti = trailArg[List[String]]()
      verify()
    }
    conf.trailingSingle() shouldBe "alpha"
    conf.trailingMulti() shouldBe List("beta", "gamma")
  }

  test ("non-required trailing option after flag") {
    object conf extends ScallopConf(Seq("-a")) {
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

  test ("trailing args separator before trailing args") {
    object conf extends ScallopConf(Seq("--", "alpha", "beta")) {
      val trailing = trailArg[List[String]]()
      verify()
    }
    conf.trailing() shouldBe List("alpha", "beta")
  }

  test ("trailing args separator inside trailing args") {
    object conf extends ScallopConf(Seq("alpha", "--", "beta")) {
      val trailing = trailArg[List[String]]()
      verify()
    }
    conf.trailing() shouldBe List("alpha", "beta")
  }

  test ("multiple trailing args separators") {
    object conf extends ScallopConf(Seq("--", "alpha", "--", "beta")) {
      val trailing = trailArg[List[String]]()
      verify()
    }
    conf.trailing() shouldBe List("alpha", "--", "beta")
  }

  test ("trailing args separator after a multi-arg option") {
    object conf extends ScallopConf(Seq("--apples", "a", "b", "c", "--", "alpha", "beta", "gamma")) {
      val apples = opt[List[String]]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    conf.apples() shouldBe List("a", "b", "c")
    conf.trailing() shouldBe List("alpha", "beta", "gamma")
  }

  test ("trailing arg before flag option") {
    object conf extends ScallopConf(Seq("beta", "-a")) {
      val apple = opt[Boolean]()
      val trailing = trailArg[String]()
      verify()
    }
    conf.apple() shouldBe true
    conf.trailing() shouldBe "beta"
  }

  test ("trailing args before and after flag option") {
    object conf extends ScallopConf(Seq("beta", "-a", "gamma")) {
      val apple = opt[Boolean]()
      val trailingBeta = trailArg[String]()
      val trailingGamma = trailArg[String]()
      verify()
    }
    conf.apple() shouldBe true
    conf.trailingBeta() shouldBe "beta"
    conf.trailingGamma() shouldBe "gamma"
  }

  test ("trailing args before and after flag option, with trailing args separator before last argument") {
    object conf extends ScallopConf(Seq("beta", "-a", "--", "gamma")) {
      val apple = opt[Boolean]()
      val trailingBeta = trailArg[String]()
      val trailingGamma = trailArg[String]()
      verify()
    }
    conf.apple() shouldBe true
    conf.trailingBeta() shouldBe "beta"
    conf.trailingGamma() shouldBe "gamma"
  }

  test ("trailing multi-arg before flag option") {
    object conf extends ScallopConf(Seq("beta", "gamma", "-a")) {
      val apple = opt[Boolean]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    conf.apple() shouldBe true
    conf.trailing() shouldBe List("beta", "gamma")
  }

  test ("trailing multi-arg before and after flag option") {
    object conf extends ScallopConf(Seq("beta", "-a", "gamma")) {
      val apple = opt[Boolean]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    conf.apple() shouldBe true
    conf.trailing() shouldBe List("beta", "gamma")
  }

  test ("trailing arg before single-arg option") {
    object conf extends ScallopConf(Seq("beta", "-a", "42")) {
      val apples = opt[Int]()
      val trailing = trailArg[String]()
      verify()
    }
    conf.apples() shouldBe 42
    conf.trailing() shouldBe "beta"
  }

  test ("trailing args before and after single-arg option") {
    object conf extends ScallopConf(Seq("beta", "-a", "42", "gamma")) {
      val apples = opt[Int]()
      val trailingBeta = trailArg[String]()
      val trailingGamma = trailArg[String]()
      verify()
    }
    conf.apples() shouldBe 42
    conf.trailingBeta() shouldBe "beta"
    conf.trailingGamma() shouldBe "gamma"
  }

  test ("trailing multi-arg before single-arg option") {
    object conf extends ScallopConf(Seq("beta", "gamma", "-a", "42")) {
      val apples = opt[Int]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    conf.apples() shouldBe 42
    conf.trailing() shouldBe List("beta", "gamma")
  }

  test ("trailing multi-arg before and after single-arg option") {
    object conf extends ScallopConf(Seq("beta", "-a", "42", "gamma")) {
      val apples = opt[Int]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    conf.apples() shouldBe 42
    conf.trailing() shouldBe List("beta", "gamma")
  }


  test ("trailing arg before multi-arg option") {
    object conf extends ScallopConf(Seq("beta", "-a", "6", "7", "42")) {
      val apples = opt[List[Int]]()
      val trailing = trailArg[String]()
      verify()
    }
    conf.apples() shouldBe List(6, 7, 42)
    conf.trailing() shouldBe "beta"
  }

  test ("trailing args before and after multi-arg option") {
    object conf extends ScallopConf(Seq("beta", "-a", "6", "42", "gamma")) {
      val apples = opt[List[Int]]()
      val trailingBeta = trailArg[String]()
      val trailingGamma = trailArg[String]()
      verify()
    }
    conf.apples() shouldBe List(6, 42)
    conf.trailingBeta() shouldBe "beta"
    conf.trailingGamma() shouldBe "gamma"
  }

  test ("trailing multi-arg before multi-arg option") {
    object conf extends ScallopConf(Seq("beta", "gamma", "-a", "7", "42")) {
      val apples = opt[List[Int]]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    conf.apples() shouldBe List(7, 42)
    conf.trailing() shouldBe List("beta", "gamma")
  }

  test ("trailing multi-arg before and after multi-arg option") {
    object conf extends ScallopConf(Seq("13", "-a", "1", "42", "gamma")) {
      val apples = opt[List[Int]]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    conf.apples() shouldBe List(1, 42)
    conf.trailing() shouldBe List("13", "gamma")
  }

  test ("trailing arg after and before flag options") {
    object conf extends ScallopConf(Seq("-a", "gamma", "-b")) {
      val apples = opt[Boolean]()
      val bananas = opt[Boolean]()
      val trailing = trailArg[String]()
      verify()
    }
    conf.apples() shouldBe true
    conf.bananas() shouldBe true
    conf.trailing() shouldBe "gamma"
  }

  test ("trailing arg after and before single-arg options") {
    object conf extends ScallopConf(Seq("-a", "42", "gamma", "-b", "53")) {
      val apples = opt[Int]()
      val bananas = opt[Int]()
      val trailing = trailArg[String]()
      verify()
    }
    conf.apples() shouldBe 42
    conf.bananas() shouldBe 53
    conf.trailing() shouldBe "gamma"
  }

  test ("trailing args: error if provided after multi-arg option and not at the end of the argument list") {
    expectException(WrongOptionFormat("apples", "42 gamma", "wrong arguments format")) {
      object conf extends ScallopConf(Seq("-a", "42", "gamma", "-b")) {
        val apples = opt[List[Int]]()
        val bananas = opt[Boolean]()
        val trailing = trailArg[String](required = false)
        verify()
      }
      conf
    }
  }

  test ("trailing args misc tests") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      val opt1 = opt[String]()
      val opt2 = opt[String]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    val conf1 = new Conf("--opt1 value TRAILING ARGS --opt2 value".split(" ").toSeq)
    conf1.opt1() shouldBe "value"
    conf1.opt2() shouldBe "value"
    conf1.trailing() shouldBe List("TRAILING", "ARGS")
    val conf2 = new Conf("TRAILING ARGS --opt1 value --opt2 value".split(" ").toSeq)
    conf2.opt1() shouldBe "value"
    conf2.opt2() shouldBe "value"
    conf2.trailing() shouldBe List("TRAILING", "ARGS")
    val conf3 = new Conf("--opt1 value --opt2 value TRAILING ARGS".split(" ").toSeq)
    conf3.opt1() shouldBe "value"
    conf3.opt2() shouldBe "value"
    conf3.trailing() shouldBe List("TRAILING", "ARGS")
  }

}
