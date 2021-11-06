package org.rogach.scallop

import org.rogach.scallop.exceptions._

class ToggleOptionTest extends ScallopTestBase {

  test ("short name") {
    object Conf extends ScallopConf(Seq("-e")) {
      val answer = toggle(name = "tgl-option", short = 'e', default = Some(false))
      verify()
    }
    Conf.answer() shouldBe true
  }

  test ("unknown option") {
    assertThrows[UnknownOption] {
      object Conf extends ScallopConf(Seq("-t")) {
        val answer = toggle(name = "tgl-option", short = 'e', default = Some(false))
        verify()
      }
      Conf
    }
  }

  test ("positive, long") {
    object Conf extends ScallopConf(Seq("--verbose")) {
      val verbose = toggle("verbose")
      verify()
    }
    Conf.verbose() should equal (true)
    Conf.verbose.isSupplied should equal (true)
  }

  test ("negative, long") {
    object Conf extends ScallopConf(Seq("--noverbose")) {
      val verbose = toggle("verbose")
      verify()
    }
    Conf.verbose() should equal (false)
    Conf.verbose.isSupplied should equal (true)
  }

  test ("short name 2") {
    object Conf extends ScallopConf(Seq("-v")) {
      val verbose = toggle("verbose")
      verify()
    }
    Conf.verbose() should equal (true)
    Conf.verbose.isSupplied should equal (true)
  }

  test ("not supplied") {
    object Conf extends ScallopConf(Seq()) {
      val verbose = toggle("verbose")
      verify()
    }
    Conf.verbose.toOption should equal (None)
    Conf.verbose.isSupplied should equal (false)
  }

  test ("not supplied, with default") {
    object Conf extends ScallopConf(Seq()) {
      val verbose = toggle("verbose", default = Some(true))
      verify()
    }
    Conf.verbose.toOption should equal (Some(true))
    Conf.verbose.isSupplied should equal (false)
  }

  test ("required, supplied") {
    object Conf extends ScallopConf(Seq("--verbose")) {
      val verbose = toggle("verbose", required = true)
      verify()
    }
    Conf.verbose.toOption shouldBe Some(true)
    Conf.verbose.isSupplied shouldBe true
  }

  test ("required, not supplied") {
    expectException(RequiredOptionNotFound("verbose")) {
      object Conf extends ScallopConf(Nil) {
        val verbose = toggle("verbose", required = true)
        verify()
      }
      Conf
    }
  }

}
