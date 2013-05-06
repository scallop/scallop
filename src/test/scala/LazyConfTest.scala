package org.rogach.scallop

import exceptions._

class LazyConfTest extends UsefulMatchers {

  test ("fail to get option value without initialization") {
    val conf = new LazyScallopConf(Seq("-a", "1")) {
      val apples = opt[Int]("apples")
    }
    intercept[IncompleteBuildException] {
      conf.apples()
    }
  }

  test ("catch exit on help") {
    var s = false
    val conf = new LazyScallopConf(Seq("--help")) {}
    conf.initialize {
      case Help("") => s = true
    }
    s ==== true
  }

  test ("catch exit on version") {
    var s = false
    val conf = new LazyScallopConf(Seq("--version")) {
      version("0.1.2.3")
    }
    conf.initialize {
      case Version => s = true
    }
    s ==== true
  }

  test ("catch exit") {
    var s = false
    val conf = new LazyScallopConf(Seq("--version")) {
      version("0.1.2.3")
    }
    conf.initialize {
      case Exit() => s = true
    }
    s ==== true
  }

  test ("catch all exceptions") {
    var s = false
    new LazyScallopConf(Seq()) {
      val apples = opt[Int]("apples", required = true)
    }.initialize {
      case ScallopException(_) => s = true
    }
    s ==== true
  }

}
