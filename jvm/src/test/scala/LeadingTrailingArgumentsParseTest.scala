package org.rogach.scallop

import org.rogach.scallop.exceptions._

class LeadingTrailingArgumentsParseTest extends ScallopTestBase {

  test ("trailing arg before flag option") {
    object Conf extends ScallopConf(Seq("beta", "-a")) {
      val apple = opt[Boolean]()
      val trailing = trailArg[String]()
      verify()
    }
    Conf.apple() shouldBe true
    Conf.trailing() shouldBe "beta"
  }

  test ("trailing args before and after flag option") {
    object Conf extends ScallopConf(Seq("beta", "-a", "gamma")) {
      val apple = opt[Boolean]()
      val trailingBeta = trailArg[String]()
      val trailingGamma = trailArg[String]()
      verify()
    }
    Conf.apple() shouldBe true
    Conf.trailingBeta() shouldBe "beta"
    Conf.trailingGamma() shouldBe "gamma"
  }

  test ("trailing args before and after flag option, with trailing args separator before last argument") {
    object Conf extends ScallopConf(Seq("beta", "-a", "--", "gamma")) {
      val apple = opt[Boolean]()
      val trailingBeta = trailArg[String]()
      val trailingGamma = trailArg[String]()
      verify()
    }
    Conf.apple() shouldBe true
    Conf.trailingBeta() shouldBe "beta"
    Conf.trailingGamma() shouldBe "gamma"
  }

  test ("trailing multi-arg before flag option") {
    object Conf extends ScallopConf(Seq("beta", "gamma", "-a")) {
      val apple = opt[Boolean]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    Conf.apple() shouldBe true
    Conf.trailing() shouldBe List("beta", "gamma")
  }

  test ("trailing multi-arg before and after flag option") {
    object Conf extends ScallopConf(Seq("beta", "-a", "gamma")) {
      val apple = opt[Boolean]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    Conf.apple() shouldBe true
    Conf.trailing() shouldBe List("beta", "gamma")
  }

  test ("trailing arg before single-arg option") {
    object Conf extends ScallopConf(Seq("beta", "-a", "42")) {
      val apples = opt[Int]()
      val trailing = trailArg[String]()
      verify()
    }
    Conf.apples() shouldBe 42
    Conf.trailing() shouldBe "beta"
  }

  test ("trailing args before and after single-arg option") {
    object Conf extends ScallopConf(Seq("beta", "-a", "42", "gamma")) {
      val apples = opt[Int]()
      val trailingBeta = trailArg[String]()
      val trailingGamma = trailArg[String]()
      verify()
    }
    Conf.apples() shouldBe 42
    Conf.trailingBeta() shouldBe "beta"
    Conf.trailingGamma() shouldBe "gamma"
  }

  test ("trailing multi-arg before single-arg option") {
    object Conf extends ScallopConf(Seq("beta", "gamma", "-a", "42")) {
      val apples = opt[Int]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    Conf.apples() shouldBe 42
    Conf.trailing() shouldBe List("beta", "gamma")
  }

  test ("trailing multi-arg before and after single-arg option") {
    object Conf extends ScallopConf(Seq("beta", "-a", "42", "gamma")) {
      val apples = opt[Int]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    Conf.apples() shouldBe 42
    Conf.trailing() shouldBe List("beta", "gamma")
  }


  test ("trailing arg before multi-arg option") {
    object Conf extends ScallopConf(Seq("beta", "-a", "6", "7", "42")) {
      val apples = opt[List[Int]]()
      val trailing = trailArg[String]()
      verify()
    }
    Conf.apples() shouldBe List(6, 7, 42)
    Conf.trailing() shouldBe "beta"
  }

  test ("trailing args before and after multi-arg option") {
    object Conf extends ScallopConf(Seq("beta", "-a", "6", "42", "gamma")) {
      val apples = opt[List[Int]]()
      val trailingBeta = trailArg[String]()
      val trailingGamma = trailArg[String]()
      verify()
    }
    Conf.apples() shouldBe List(6, 42)
    Conf.trailingBeta() shouldBe "beta"
    Conf.trailingGamma() shouldBe "gamma"
  }

  test ("trailing multi-arg before multi-arg option") {
    object Conf extends ScallopConf(Seq("beta", "gamma", "-a", "7", "42")) {
      val apples = opt[List[Int]]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    Conf.apples() shouldBe List(7, 42)
    Conf.trailing() shouldBe List("beta", "gamma")
  }

  test ("trailing multi-arg before and after multi-arg option") {
    object Conf extends ScallopConf(Seq("13", "-a", "1", "42", "gamma")) {
      val apples = opt[List[Int]]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    Conf.apples() shouldBe List(1, 42)
    Conf.trailing() shouldBe List("13", "gamma")
  }

  test ("trailing arg after and before flag options") {
    object Conf extends ScallopConf(Seq("-a", "gamma", "-b")) {
      val apples = opt[Boolean]()
      val bananas = opt[Boolean]()
      val trailing = trailArg[String]()
      verify()
    }
    Conf.apples() shouldBe true
    Conf.bananas() shouldBe true
    Conf.trailing() shouldBe "gamma"
  }

  test ("trailing arg after and before single-arg options") {
    object Conf extends ScallopConf(Seq("-a", "42", "gamma", "-b", "53")) {
      val apples = opt[Int]()
      val bananas = opt[Int]()
      val trailing = trailArg[String]()
      verify()
    }
    Conf.apples() shouldBe 42
    Conf.bananas() shouldBe 53
    Conf.trailing() shouldBe "gamma"
  }

  test ("trailing args: error if provided after multi-arg option and not at the end of the argument list") {
    expectException(WrongOptionFormat("apples", "42 gamma", "java.lang.NumberFormatException: For input string: \"gamma\"")) {
      object Conf extends ScallopConf(Seq("-a", "42", "gamma", "-b")) {
        val apples = opt[List[Int]]()
        val bananas = opt[Boolean]()
        val trailing = trailArg[String](required = false)
        verify()
      }
      Conf
    }
  }

  test ("leading trailing args misc tests") {
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
