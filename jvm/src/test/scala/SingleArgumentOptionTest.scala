package org.rogach.scallop

class SingleArgumentOptionTest extends ScallopTestBase {

  test ("one integer option, non-required and missing") {
    object Conf extends ScallopConf(Seq()) {
      val apples = opt[Int]("apples")
      verify()
    }
    Conf.apples.toOption shouldBe None
  }

  test ("one integer option, short form") {
    object Conf extends ScallopConf(Seq("-a", "42")) {
      val apples = opt[Int]("apples")
      verify()
    }
    Conf.apples() shouldBe 42
    Conf.apples.toOption shouldBe Some(42)
  }

  test ("one integer option, long form") {
    object Conf extends ScallopConf(Seq("--apples", "42")) {
      val apples = opt[Int]("apples")
      verify()
    }
    Conf.apples() shouldBe 42
    Conf.apples.toOption shouldBe Some(42)
  }

  test ("--arg=value option style") {
    object Conf extends ScallopConf(Seq("--apples=42")) {
      val apples = opt[Int]()
      verify()
    }

    Conf.apples() shouldBe 42
  }

  test ("pass arguments that start with dash") {
    object Conf extends ScallopConf(Seq("--apples=-1")) {
      val apples = opt[Int]()
      verify()
    }

    Conf.apples() shouldBe (-1)
  }

  test ("one string option") {
    object Conf extends ScallopConf(Seq("--apples", "abc")) {
      val apples = opt[String]("apples")
      verify()
    }
    Conf.apples() shouldBe "abc"
  }

  test ("one short-integer option") {
    object Conf extends ScallopConf(Seq("--apples", "42")) {
      val apples = opt[Short]("apples")
      verify()
    }
    Conf.apples() shouldBe 42
  }

  test ("one byte option") {
    object Conf extends ScallopConf(Seq("--apples", "42")) {
      val apples = opt[Byte]("apples")
      verify()
    }
    Conf.apples() shouldBe 42
  }

  test ("one double-precision float option") {
    object Conf extends ScallopConf(Seq("--weight", "42.1")) {
      val weight = opt[Double]("weight")
      verify()
    }
    Conf.weight() shouldBe 42.1
  }

  test ("default value") {
    object Conf extends ScallopConf(Seq()) {
      val answer = opt[Int]("answer", default = Some(42), required = true)
      verify()
    }
    Conf.answer() shouldBe 42
  }

  test ("negative number in option arguments") {
    object Conf extends ScallopConf(Seq("-a", "-1")) {
      val apples = opt[Int]("apples")
      verify()
    }
    Conf.apples() shouldBe (-1)
  }

  test ("short option with arg concatenation") {
    object Conf extends ScallopConf(Seq("-ffile")) {
      val file = opt[String]("file")
      verify()
    }
    Conf.file() should equal ("file")
  }

  test ("default value of option should be lazily evaluated") {
    object Conf extends ScallopConf(Seq("-a", "4")) {
      val apples = opt[Int](default = { sys.error("boom"); None })
      verify()
    }
    Conf.apples() should equal (4)
  }

  test ("hyphens as arguments") {
    object Conf extends ScallopConf(Seq("--output", "-", "-")) {
      val output = opt[String]("output")
      val input = trailArg[List[String]]("input")
      verify()
    }
    Conf.output() should equal ("-")
    Conf.input() should equal (List("-"))
  }

}
