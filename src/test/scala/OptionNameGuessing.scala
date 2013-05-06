package org.rogach.scallop

import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class OptionNameGuessing extends UsefulMatchers {
  throwError.value = true

  test ("simple") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      val appleso = opt[Int]()
      val bananaso = opt[Int]()
    }
    Conf.appleso() should equal (1)
    Conf.bananaso.get should equal (None)
  }

  test ("tricky") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      val appleso = opt[Int]()
      val applesPlus = appleso.map(2+)
      lazy val applesVal = appleso()
      val bananaso = opt[Int]()
      val aaa = opt[Int]()
    }
    Conf.appleso() should equal (1)
    Conf.bananaso.get should equal (None)
    Conf.aaa.get should equal (None)
  }

  test ("camelCase convert") {
    object Conf extends ScallopConf(Seq("--apple-treeo", "1")) {
      val appleTreeo = opt[Int]()
    }
    Conf.appleTreeo() should equal (1)
  }

  test ("guessing in subcommands") {
    object Conf extends ScallopConf(Seq("tree", "--apples", "3")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Int]()
      }
    }
    Conf.tree.apples() should equal (3)
  }

  test ("replacing of the name in mapped ScallopOptions") {
    object Conf extends ScallopConf(Nil) {
      val xs = opt[Int](default = Some(0)) map (1+)
    }
    Conf.xs.get ==== Some(1)
  }

  test ("replacing of the name in doubly mapped ScallopOptions") {
    object Conf extends ScallopConf(Nil) {
      val xs = opt[Int](default = Some(0)) map (1+) map (_.toString)
    }
    Conf.xs.get ==== Some("1")
  }

  test("trailArg name guessing") {
    object Conf extends ScallopConf(List("1", "foo", "bar", "baz", "bippy")) {
      val trailing1 = trailArg[Int]()
      val trailing2 = trailArg[List[String]]()
      verify()
    }
    Conf.trailing1.name should be ("trailing1")
    Conf.trailing2.name should be ("trailing2")
    Conf.trailing1.get should be (Some(1))
    Conf.trailing2.get should be (Some(List("foo", "bar", "baz", "bippy")))
  }

  test("toggle name guessing") {
    object Conf extends ScallopConf(List("--foo", "--nobippy", "-s")) {
      val foo = toggle()
      val zoop = toggle()
      val bippy = toggle()
      val scooby = toggle()
    }
    Conf.foo.name should be ("foo")
    Conf.zoop.name should be ("zoop")
    Conf.bippy.name should be ("bippy")
    Conf.foo.get should be (Some(true))
    Conf.zoop.get should be (None)
    Conf.bippy.get should be (Some(false))
    Conf.scooby.get should be (Some(true))
  }

}
