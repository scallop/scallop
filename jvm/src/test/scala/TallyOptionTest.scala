package org.rogach.scallop

import org.rogach.scallop.exceptions._

class TallyOptionTest extends ScallopTestBase {

  test ("empty tally") {
    object Conf extends ScallopConf(Seq()) {
      val apples = tally()
      verify()
    }
    Conf.apples() should equal (0)
    Conf.apples.isSupplied should equal (false)
  }

  test ("one-arg tally") {
    object Conf extends ScallopConf(Seq("-a")) {
      val apples = tally()
      verify()
    }
    Conf.apples() should equal (1)
  }

  test ("two-arg tally") {
    object Conf extends ScallopConf(Seq("-a", "-a")) {
      val apples = tally()
      verify()
    }
    Conf.apples() should equal (2)
  }

  test ("collapsed two-arg tally") {
    object Conf extends ScallopConf(Seq("-aa")) {
      val apples = tally()
      verify()
    }
    Conf.apples() should equal (2)
  }

  test ("tally no-args") {
    throwError.withValue(true) {
      expectException(ExcessArguments(List("stuff"))) {
        object Conf extends ScallopConf(Seq("-a", "stuff", "--verbose")) {
          val apples = tally()
          val verbose = opt[Boolean]()
          verify()
        }
        Conf
      }
    }
  }

}
