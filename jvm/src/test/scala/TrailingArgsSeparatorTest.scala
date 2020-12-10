package org.rogach.scallop

class TrailingArgsSeparatorTest extends ScallopTestBase {

  test ("trailing args separator after one-arg option") {
    object Conf extends ScallopConf(Seq("-a","1","--","-b","2")) {
      val apples = opt[Int]("apples")
      val bananas = trailArg[List[String]]("bananas")
      verify()
    }
    Conf.apples() should equal (1)
    Conf.bananas() should equal (List("-b", "2"))
  }

  test ("trailing args separator before trailing args") {
    object Conf extends ScallopConf(Seq("--", "alpha", "beta")) {
      val trailing = trailArg[List[String]]()
      verify()
    }
    Conf.trailing() shouldBe List("alpha", "beta")
  }

  test ("trailing args separator inside trailing args") {
    object Conf extends ScallopConf(Seq("alpha", "--", "beta")) {
      val trailing = trailArg[List[String]]()
      verify()
    }
    Conf.trailing() shouldBe List("alpha", "beta")
  }

  test ("multiple trailing args separators") {
    object Conf extends ScallopConf(Seq("--", "alpha", "--", "beta")) {
      val trailing = trailArg[List[String]]()
      verify()
    }
    Conf.trailing() shouldBe List("alpha", "--", "beta")
  }

  test ("trailing args separator after a multi-arg option") {
    object Conf extends ScallopConf(Seq("--apples", "a", "b", "c", "--", "alpha", "beta", "gamma")) {
      val apples = opt[List[String]]()
      val trailing = trailArg[List[String]]()
      verify()
    }
    Conf.apples() shouldBe List("a", "b", "c")
    Conf.trailing() shouldBe List("alpha", "beta", "gamma")
  }

}
