package org.rogach.scallop

import org.rogach.scallop.exceptions._

class ConfTest extends ScallopTestBase {

  test ("passing to functions") {
    object Conf extends ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify()
    }
    def a(conf: Conf.type): Unit = {
      conf.apples.toOption should equal (Some(3))
    }
    a(Conf)
  }

  test ("extracting values before call to verify") {
    expectException(IncompleteBuildException()) {
      object Conf extends ScallopConf(List("-a")) {
        val apples = opt[Boolean]("apples")
        apples()
        verify()
      }
      Conf
    }
  }

  test ("using mixins") {
    class CommonOptions(args: Seq[String]) extends ScallopConf(args) {
      val fruits = opt[Int]()
    }

    trait AppleOption { self: ScallopConf =>
      val apples = opt[Int]()
    }

    trait BananaOption { self: ScallopConf =>
      val bananas = opt[Int]()
    }

    class FooOptions(args: Seq[String]) extends CommonOptions(args) with AppleOption
    class BarOptions(args: Seq[String]) extends CommonOptions(args) with AppleOption with BananaOption

    val foo = new FooOptions(Seq("--apples", "42"))
    foo.verify()
    foo.apples() shouldBe 42

    val bar = new BarOptions(Seq("--apples", "42", "--bananas", "43"))
    bar.verify()
    bar.apples() shouldBe 42
    bar.bananas() shouldBe 43
  }

}
