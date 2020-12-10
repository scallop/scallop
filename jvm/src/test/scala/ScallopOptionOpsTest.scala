package org.rogach.scallop

class ScallopOptionOpsTest extends ScallopTestBase {

  test ("printing ScallopOption") {
    object Conf extends ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify()
    }
    Conf.apples.toString should equal ("ScallopSome(3)")
  }

  test ("toggle flag option") {
    object Conf extends ScallopConf(List("-a")) {
      val apples = opt[Boolean]("apples").map(!_)
      val bananas = opt[Boolean]("bananas").map(!_)
      verify()
    }
    Conf.apples() should equal (false)
    Conf.bananas() should equal (true)
  }

  test ("option operations - all operations") {
    object Conf extends ScallopConf(List("-a","3","-b","5")) {
      val apples = opt[Int]("apples")
      val applesCollect = apples.collect({case a:Int => a + 1})
      val applesFilter1 = apples.filter(2<)
      val applesFilter2 = apples.filter(5<)
      val applesFilterNot = apples.filterNot(5<)
      val applesMap1 = apples.map(2+)
      val applesMap2 = apples.filter(5<).map(2+)
      val applesOrElse1 = apples.orElse(Some(1))
      val applesOrElse2 = apples.filter(5<).orElse(Some(1))
      val bananas = opt[String]("bananas").collect({case b:Int => b + 1}:PartialFunction[Any,Int])
      verify()
    }
    Conf.applesCollect.toOption should equal (Some(4))
    Conf.applesFilter1.toOption should equal (Some(3))
    Conf.applesFilter2.toOption should equal (None)
    Conf.applesFilterNot.toOption should equal (Some(3))
    Conf.applesMap1.toOption should equal (Some(5))
    Conf.applesMap2.toOption should equal (None)
    Conf.applesOrElse1.toOption should equal (Some(3))
    Conf.applesOrElse2.toOption should equal (Some(1))
    Conf.bananas.toOption should equal (None)
  }

  test ("for comprehensions with ScallopOptions") {
    object Conf extends ScallopConf(Seq("-a","3","-b","2")) {
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val weight = for {
        a <- apples
        if a > 2
        b <- bananas
      } yield a * 2 + b * 3
      val weight2 = for { a <- apples; if a < 2; b <- bananas } yield a * 2 + b * 3
      verify()
    }
    Conf.weight.toOption should equal (Some(12))
    Conf.weight2.toOption should equal (None)
  }


}
