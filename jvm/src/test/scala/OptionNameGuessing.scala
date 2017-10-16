package org.rogach.scallop

import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class OptionNameGuessing extends UsefulMatchers {
  throwError.value = true

  test ("simple") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      val appleso = opt[Int]()
      val bananaso = opt[Int]()

      verify()
    }
    Conf.appleso() should equal (1)
    Conf.bananaso.toOption should equal (None)
  }

  test ("shadowing with derived option") {
    object Conf extends ScallopConf(Seq("--apples", "1")) {
      val apples = opt[Int]()
      val applesPlus = apples.map(2+)

      verify()
    }
    Conf.apples() should equal (1)
    Conf.applesPlus() should equal (3)
  }

  test ("tricky") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      val appleso = opt[Int]()
      val applesPlus = appleso.map(2+)
      lazy val applesVal = appleso()
      val bananaso = opt[Int]()
      val aaa = opt[Int]()

      verify()
    }
    Conf.appleso() should equal (1)
    Conf.applesPlus() should equal (3)
    Conf.bananaso.toOption should equal (None)
    Conf.aaa.toOption should equal (None)
  }

  test ("camelCase convert") {
    object Conf extends ScallopConf(Seq("--apple-treeo", "1")) {
      val appleTreeo = opt[Int]()

      verify()
    }
    Conf.appleTreeo() should equal (1)
  }

  test ("guessing in subcommands") {
    object Conf extends ScallopConf(Seq("tree", "--apples", "3")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Int]()
      }
      addSubcommand(tree)

      verify()
    }
    Conf.tree.apples() should equal (3)
  }

  test ("replacing of the name in mapped ScallopOptions") {
    object Conf extends ScallopConf(Nil) {
      val xs = opt[Int](default = Some(0)) map (1+)

      verify()
    }
    Conf.xs.toOption ==== Some(1)
  }

  test ("replacing of the name in doubly mapped ScallopOptions") {
    object Conf extends ScallopConf(Nil) {
      val xs = opt[Int](default = Some(0)) map (1+) map (_.toString)

      verify()
    }
    Conf.xs.toOption ==== Some("1")
  }

  test("trailArg name guessing") {
    object Conf extends ScallopConf(List("1", "foo", "bar", "baz", "bippy")) {
      val trailing1 = trailArg[Int]()
      val trailing2 = trailArg[List[String]]()

      verify()
    }
    Conf.trailing1.name should be ("trailing1")
    Conf.trailing2.name should be ("trailing2")
    Conf.trailing1.toOption should be (Some(1))
    Conf.trailing2.toOption should be (Some(List("foo", "bar", "baz", "bippy")))
  }

  test("toggle name guessing") {
    object Conf extends ScallopConf(List("--foo", "--nobippy", "-s")) {
      val foo = toggle()
      val zoop = toggle()
      val bippy = toggle()
      val scooby = toggle()

      verify()
    }
    Conf.foo.name should be ("foo")
    Conf.zoop.name should be ("zoop")
    Conf.bippy.name should be ("bippy")
    Conf.foo.toOption should be (Some(true))
    Conf.zoop.toOption should be (None)
    Conf.bippy.toOption should be (Some(false))
    Conf.scooby.toOption should be (Some(true))
  }

  test("decode special symbols") {
    object Conf extends ScallopConf(List()) {
      val `source-folder` = opt[String]()
      verify()
    }
    Conf.`source-folder`.name shouldBe "source-folder"
  }

}
