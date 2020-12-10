package org.rogach.scallop

class ScallopOptionIsSuppliedTest extends ScallopTestBase {

  test ("option value was supplied") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      val apples = opt[Int]("apples")
      verify()
    }
    Conf.apples.isSupplied shouldBe true
  }

  test ("option value was not supplied, no default") {
    object Conf extends ScallopConf(Seq()) {
      val apples = opt[Int]("apples")
      verify()
    }
    Conf.apples.isSupplied shouldBe false
  }

  test ("option value was not supplied, with default") {
    object Conf extends ScallopConf(Seq()) {
      val apples = opt[Int]("apples", default = Some(7))
      verify()
    }
    Conf.apples.isSupplied shouldBe false
  }

  test ("trail arg value was supplied") {
    object Conf extends ScallopConf(Seq("first")) {
      val file = trailArg[String]("file", required = false)
      verify()
    }
    Conf.file.isSupplied shouldBe true
  }

  test ("trail arg value was not supplied, no default value") {
    object Conf extends ScallopConf(Seq()) {
      val file = trailArg[Int]("file", required = false)
      verify()
    }
    Conf.file.isSupplied shouldBe false
  }

  test ("trail arg value was not supplied, with default value") {
    object Conf extends ScallopConf(Seq()) {
      val file = trailArg[String]("file", required = false, default = Some("second"))
      verify()
    }
    Conf.file.isSupplied shouldBe false
  }

  test ("isSupplied on transformed option with guessed option name") {
    object Config extends ScallopConf(Seq("-i", "5")) {
      val index = opt[Int]().map(_-1)
      verify()
    }
    Config.index.isSupplied shouldBe true
  }

  test ("isSupplied on transformed option with guessed option name inside validation") {
    object Config extends ScallopConf(Seq("-i", "5", "-l", "10")) {
      val index = opt[Int]().map(_-1)
      val length = opt[Int]()
      addValidation {
        if (index.isSupplied && length.isSupplied && index() >= length()) {
          Left("Index out of bounds")
        } else Right(())
      }
      verify()
    }
    Config.index.isSupplied shouldBe true
  }

}
