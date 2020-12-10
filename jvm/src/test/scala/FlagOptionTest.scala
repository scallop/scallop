package org.rogach.scallop

class FlagOptionTest extends ScallopTestBase {

  test ("simple flag - short form") {
    object Conf extends ScallopConf(Seq("-a")) {
      val apples = opt[Boolean]("apples")
      verify()
    }
    Conf.apples() shouldBe true
  }

  test ("simple flag - short form, explicitly changed short name") {
    object Conf extends ScallopConf(Seq("-b")) {
      val apples = opt[Boolean]("apples", short = 'b')
      verify()
    }
    Conf.apples() shouldBe true
  }

  test ("simple flag - long form") {
    object Conf extends ScallopConf(Seq("--apples")) {
      val apples = opt[Boolean]("apples")
      verify()
    }
    Conf.apples() shouldBe true
  }

  test ("simple flag - not provided") {
    object Conf extends ScallopConf(Seq()) {
      val apples = opt[Boolean]("apples")
      verify()
    }
    Conf.apples() shouldBe false
  }

  test ("two short flags, implicit short names") {
    object Conf extends ScallopConf(Seq("-a")) {
      val apples = opt[Boolean]("apples")
      val bananas = opt[Boolean]("bananas")
      verify()
    }
    Conf.apples() shouldBe true
    Conf.bananas() shouldBe false
  }

  test ("two short flags, implicit short names, required value") {
    object Conf extends ScallopConf(Seq("-a")) {
      val apples = opt[Boolean]("apples", required = true)
      val bananas = opt[Boolean]("bananas")
      verify()
    }
    Conf.apples() shouldBe true
    Conf.bananas() shouldBe false
  }

  test ("boolean default value") {
    object Conf extends ScallopConf(List("-b")) {
      val apples = opt[Boolean]("apples", default = Some(true))
      val bananas = opt[Boolean]("bananas", default = Some(false))
      verify()
    }
    Conf.apples() should equal (true)
    Conf.bananas() should equal (true)
  }

}
