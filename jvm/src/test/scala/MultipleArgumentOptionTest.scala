package org.rogach.scallop

class MultipleArgumentOptionTest extends ScallopTestBase {

  test ("list of strings, one argument") {
    object Conf extends ScallopConf(Seq("--numbers", "alpha")) {
      val numbers = opt[List[String]]("numbers")
      verify()
    }
    Conf.numbers() shouldBe List("alpha")
  }

  test ("list of strings, three arguments") {
    object Conf extends ScallopConf(Seq("--numbers", "alpha", "beta", "gamma")) {
      val numbers = opt[List[String]]("numbers")
      verify()
    }
    Conf.numbers() shouldBe List("alpha", "beta", "gamma")
  }

  test ("list of ints") {
    object Conf extends ScallopConf(Seq("--numbers", "42", "12", "345")) {
      val numbers = opt[List[Int]]("numbers")
      verify()
    }
    Conf.numbers() shouldBe List(42, 12, 345)
  }

  test ("list of doubles") {
    object Conf extends ScallopConf(Seq("--numbers", "42.0", "12", "345e0")) {
      val numbers = opt[List[Double]]("numbers")
      verify()
    }
    Conf.numbers() shouldBe List(42.0, 12.0, 345.0)
  }

  test ("if no arguments provided for list option, default should be returned") {
    object Conf extends ScallopConf(Seq()) {
      val apples = opt[List[Int]](default = Some(List(1)))
      verify()
    }
    Conf.apples() should equal (List(1))
  }

  test ("pass list of arguments in --arg=value option style") {
    object Conf extends ScallopConf(Seq("--apples=-1", "--apples=-2")) {
      val apples = opt[List[Int]]()
      verify()
    }

    Conf.apples() shouldBe List(-1, -2)
  }

  test ("empty list arg before empty trailing option") {
    object Conf extends ScallopConf(Seq("-a")) {
      val apples = opt[List[String]](default = Some(Nil))
      verify()
    }
    Conf.apples() shouldBe List()
  }

  test ("multiple list option before normal option should keep ordering") {
    object Conf extends ScallopConf(Seq("-l", "1", "-l", "2", "-o", "3")) {
      val l = opt[List[String]]()
      val o = opt[Int]()
      verify()
    }
    Conf.l() shouldBe List("1","2")
  }

  test ("multiple list option before optional trail arg should keep ordering") {
    object Conf extends ScallopConf(Seq("-l", "1", "-l", "2", "--", "0")) {
      val l = opt[List[String]]()
      val t = trailArg[Int](required = false)
      verify()
    }
    Conf.l() shouldBe List("1","2")
  }

}
