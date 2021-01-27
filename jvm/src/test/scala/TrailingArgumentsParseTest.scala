package org.rogach.scallop

import org.rogach.scallop.exceptions._

class TrailingArgumentsParseTest extends ScallopTestBase {

  test ("single one-arg trailing arg") {
    object Conf extends ScallopConf(Seq("alpha")) {
      val trailing = trailArg[String]()
      verify()
    }
    Conf.trailing() shouldBe "alpha"
  }

  test ("two one-arg trailing args") {
    object Conf extends ScallopConf(Seq("alpha", "beta")) {
      val trailingAlpha = trailArg[String]()
      val trailingBeta = trailArg[String]()
      verify()
    }
    Conf.trailingAlpha() shouldBe "alpha"
    Conf.trailingBeta() shouldBe "beta"
  }

  test ("single multi-arg trailing arg") {
    object Conf extends ScallopConf(Seq("alpha", "beta")) {
      val trailing = trailArg[List[String]]()
      verify()
    }
    Conf.trailing() shouldBe List("alpha", "beta")
  }

  test ("two trailing args: one single arg, one multi-arg") {
    object Conf extends ScallopConf(Seq("alpha", "beta", "gamma")) {
      val trailingSingle = trailArg[String]()
      val trailingMulti = trailArg[List[String]]()
      verify()
    }
    Conf.trailingSingle() shouldBe "alpha"
    Conf.trailingMulti() shouldBe List("beta", "gamma")
  }

  test ("non-required trailing option after flag, not provided") {
    object Conf extends ScallopConf(Seq("-a")) {
      val apple = opt[Boolean]("apple")
      val banana = trailArg[String](required = false)
      verify()
    }
    Conf.apple.toOption shouldBe Some(true)
    Conf.banana.toOption shouldBe None
  }

  test ("non-required trailing option after flag, provided") {
    object Conf extends ScallopConf(Seq("-a", "x")) {
      val apple = opt[Boolean]("apple")
      val banana = trailArg[String](required = false)
      verify()
    }
    Conf.apple.toOption shouldBe Some(true)
    Conf.banana.toOption shouldBe Some("x")
  }

  test ("optional trailing arguments after single flag") {
    object Conf extends ScallopConf(Seq("--echo", "42", "43")) {
      val echo = opt[Boolean]("echo")
      val numbers = trailArg[List[Int]]("numbers", required = false)
      verify()
    }
    Conf.echo() shouldBe true
    Conf.numbers() shouldBe List(42, 43)
  }

  test ("trailing arg after single short-named option") {
    object Conf extends ScallopConf(Seq("-a", "42", "rabbit")) {
      val apples = opt[Int]("apples")
      val animal = trailArg[String]("animal")
      verify()
    }
    Conf.apples() shouldBe 42
    Conf.animal() shouldBe "rabbit"
  }

  test ("trailing arg after single long-named option") {
    object Conf extends ScallopConf(Seq("--apples", "42", "rabbit")) {
      val apples = opt[Int]("apples")
      val animal = trailArg[String]("animal")
      verify()
    }
    Conf.apples() shouldBe 42
    Conf.animal() shouldBe "rabbit"
  }

  test ("trailing arg after two other options") {
    object Conf extends ScallopConf(Seq("-d", "--num-limbs", "1", "Pigeon")) {
      val donkey = opt[Boolean]("donkey")
      val numLimbs = opt[Int]("num-limbs")
      val petName = trailArg[String]("pet-name")
      verify()
    }
    Conf.donkey() shouldBe true
    Conf.numLimbs() shouldBe 1
    Conf.petName() shouldBe "Pigeon"
  }

  test ("trailing arg after single property argument") {
    object Conf extends ScallopConf(Seq("-E", "key=value", "rabbit")) {
      val e = props[String]('E')
      val animal = trailArg[String]("animal")
      verify()
    }
    Conf.e.get("key") shouldBe Some("value")
    Conf.animal() shouldBe "rabbit"
  }

  test ("trailing arg after a multi-arg option") {
    object Conf extends ScallopConf(Seq("--echo", "42", "43")) {
      val echo = opt[List[Int]]("echo")
      val numbers = trailArg[List[Int]]("numbers")
      verify()
    }
    Conf.echo() shouldBe List(42)
    Conf.numbers() shouldBe List(43)
  }

  test ("optional single-arg trailing arg after a multi-arg option") {
    object Conf extends ScallopConf(Seq("--echo", "42", "43")) {
      val echo = opt[List[Int]]("echo")
      val number = trailArg[Int]("number", required = false)
      verify()
    }
    Conf.echo() shouldBe List(42, 43)
    Conf.number.toOption shouldBe None
  }

  test ("optional multi-arg trailing arg after a multi-arg option") {
    object Conf extends ScallopConf(Seq("--echo", "42", "43")) {
      val echo = opt[List[Int]]("echo")
      val numbers = trailArg[List[Int]]("numbers", required = false)
      verify()
    }
    Conf.echo() shouldBe List(42, 43)
    Conf.numbers.toOption shouldBe None
  }

  test ("multi-arg trailing args after single property argument") {
    object Conf extends ScallopConf(Seq("-E", "key=value", "rabbit", "key2=value2")) {
      val e = props[String]('E')
      val rubbish = trailArg[List[String]]("rubbish")
      verify()
    }
    Conf.e.get("key") shouldBe Some("value")
    Conf.rubbish() shouldBe List("rabbit", "key2=value2")
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

  test ("trailing args - one required, one optional - both provided") {
    object Conf extends ScallopConf(Seq("first", "second")) {
      val name = trailArg[String]("name")
      val surname = trailArg[String]("surname", required = false)
      verify()
    }
    Conf.name() shouldBe "first"
    Conf.surname.toOption shouldBe Some("second")
  }

  test ("trailing args - one required, one optional - one provided") {
    object Conf extends ScallopConf(Seq("first")) {
      val name = trailArg[String]("name")
      val surname = trailArg[String]("surname", required = false)
      verify()
    }
    Conf.name() shouldBe "first"
    Conf.surname.toOption shouldBe None
  }

  test ("trailing arg - with default value, provided") {
    object Conf extends ScallopConf(Seq("first")) {
      val name = trailArg[String]("name", required = false, default = Some("aoeu"))
      verify()
    }
    Conf.name() shouldBe "first"
  }

  test ("trailing arg - with default value, not provided") {
    object Conf extends ScallopConf(Seq()) {
      val name = trailArg[String]("name", required = false, default = Some("aoeu"))
      verify()
    }
    Conf.name() shouldBe "aoeu"
  }

  test ("default value of trailing arg should be lazily evaluated") {
    object Conf extends ScallopConf(Seq("4")) {
      val apples = trailArg[Int](default = { sys.error("boom"); None })
      verify()
    }
    Conf.apples() should equal (4)
  }

  test ("handle trailing args in conjunction with --arg=value option style") {
    object Conf extends ScallopConf(Seq("--apples=-1", "basket")) {
      val apples = opt[Int]()
      val stuff = trailArg[String]()
      verify()
    }

    Conf.apples() shouldBe (-1)
    Conf.stuff() shouldBe "basket"
  }

  test ("negative number in trailing arguments") {
    object Config extends ScallopConf(Seq("-1234")) {
      val value = trailArg[Int]()
      verify()
    }
    Config.value() shouldBe -1234
  }

  test ("do not throw excess arguments error if there is only one multiple-arg trailing option") {
    intercept[WrongOptionFormat] {
      object Conf extends ScallopConf(Seq("1", "x")) {
        val apples = trailArg[List[Int]]("apples")
        verify()
      }
      Conf.apples() shouldBe List(1, 2)
    }
  }

}
