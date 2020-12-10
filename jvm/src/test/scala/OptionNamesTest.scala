package org.rogach.scallop

class OptionNamesTest extends ScallopTestBase {

  test ("default for props") {
    object Conf extends ScallopConf(List("-D", "foo=bar,", "bar=baz,", "baz=bippy")) {
      val properties = props[String]()
      verify()
    }
    Conf.properties should equal (Map("foo"->"bar", "bar"->"baz", "baz"->"bippy"))
  }

  test ("default name for propsLong") {
    object Conf extends ScallopConf(List("--Props", "foo=bar", "bar=baz")) {
      val properties = propsLong[String]()
      verify()
    }
    Conf.properties should equal (Map("foo" -> "bar", "bar" -> "baz"))
  }

  test ("option implicit short name clashes with property name") {
    object Conf extends ScallopConf(Seq("-D", "key=value")) {
      val d = props[String]('D')
      val dark = opt[String]("Dark")
      verify()
    }
    Conf.d.get("key") shouldBe Some("value")
    Conf.dark.toOption shouldBe None
  }

  test ("numbers in option names") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      val apples1 = opt[Int]("apples1")
      val apples2 = opt[Int]("apples2")
      verify()
    }
    Conf.apples1.toOption should equal (Some(1))
  }

}
