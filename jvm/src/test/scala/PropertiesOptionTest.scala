package org.rogach.scallop

class PropertiesOptionTest extends ScallopTestBase {

  test ("no value") {
    object Conf extends ScallopConf(Seq()) {
      val d = props[String]('D')
      verify()
    }
    Conf.d.get("aoeu") shouldBe None
  }

  test ("key=value concatenated to option invocation") {
    object Conf extends ScallopConf(Seq("-Daoeu=htns")) {
      val d = props[String]('D')
      verify()
    }
    Conf.d.get("aoeu") shouldBe Some("htns")
  }

  test ("key=value is in a separate argument") {
    object Conf extends ScallopConf(Seq("-D", "a=b")) {
      val d = props[String]('D')
      verify()
    }
    Conf.d.get("a") shouldBe Some("b")
  }

  test ("two key=value pairs") {
    object Conf extends ScallopConf(Seq("-D", "a=1", "b=2")) {
      val d = props[String]('D')
      verify()
    }
    Conf.d.get("a") shouldBe Some("1")
    Conf.d.get("b") shouldBe Some("2")
  }

  test ("two key=value pairs, one concatenated to option invocation") {
    object Conf extends ScallopConf(Seq("-Da=1", "b=2")) {
      val d = props[String]('D')
      verify()
    }
    Conf.d.get("a") shouldBe Some("1")
    Conf.d.get("b") shouldBe Some("2")
  }

  test ("get map from the option container") {
    object Conf extends ScallopConf(Seq("-D", "a=1", "b=2")) {
      val d = props[String]('D')
      verify()
    }
    Conf.d.toMap shouldBe Map("a" -> "1", "b" -> "2")
  }

  test ("overwrite key=value pairs in order of appearance") {
    object Conf extends ScallopConf(Seq("-D", "answer=41", "answer=42")) {
      val d = props[String]('D')
      verify()
    }
    Conf.d.get("answer") shouldBe Some("42")
  }

  test ("int values") {
    object Conf extends ScallopConf(Seq("-Ekey1=1", "key2=2")) {
      val d = props[Int]('E')
      verify()
    }
    Conf.d.get("key1") shouldBe Some(1)
    Conf.d.get("key2") shouldBe Some(2)
    Conf.d.toMap shouldBe Map("key1" -> 1, "key2" -> 2)
  }

  test ("double values") {
    object Conf extends ScallopConf(Seq("-Ekey1=1.1", "key2=2.3")) {
      val d = props[Double]('E')
      verify()
    }
    Conf.d.get("key1") shouldBe Some(1.1)
    Conf.d.get("key2") shouldBe Some(2.3)
    Conf.d.toMap shouldBe Map("key1" -> 1.1, "key2" -> 2.3)
  }

  test ("short-named property args with commas") {
    object Conf extends ScallopConf(Seq("-Akey1=1,key2=2")) {
      val app = props[Int]('A')
      verify()
    }
    Conf.app("key1") should equal (1)
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("short-named property args with commas and spaces") {
    object Conf extends ScallopConf(Seq("-A","key1=1",",","key2=2")) {
      val app = props[Int]('A')
      verify()
    }
    Conf.app.get("key1") should equal (Some(1))
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("short-named property args with commas and spaces 2") {
    object Conf extends ScallopConf(Seq("-A","key1=1,","key2=2")) {
      val app = props[Int]('A')
      verify()
    }
    Conf.app.get("key1") should equal (Some(1))
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("long-named property args") {
    object Conf extends ScallopConf(Seq("--Apples","key1=1","key2=2")) {
      val app = propsLong[Int]("Apples")
      verify()
    }
    Conf.app.get("key1") should equal (Some(1))
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("long-named property args with commas and spaces") {
    object Conf extends ScallopConf(Seq("--Apples","key1=1",",","key2=2")) {
      val app = propsLong[Int]("Apples")
      verify()
    }
    Conf.app.get("key1") should equal (Some(1))
    Conf.app.get("key2") should equal (Some(2))
  }

  test ("escaped commas in property args") {
    object Conf extends ScallopConf(Seq("-A", "key=1\\,2")) {
      val app = props[String]('A')
      verify()
    }
    Conf.app.get("key") shouldBe Some("1,2")
  }

  test ("escaped commas mixed with non-escaped separators in property args") {
    object Conf extends ScallopConf(Seq("-A", "key1=1\\,2,key2=3")) {
      val app = props[String]('A')
      verify()
    }
    Conf.app.get("key1") shouldBe Some("1,2")
    Conf.app.get("key2") shouldBe Some("3")
  }

  test ("escaped equals in property args") {
    object Conf extends ScallopConf(Seq("-A", "key=1\\=2")) {
      val app = props[String]('A')
      verify()
    }
    Conf.app.get("key") shouldBe Some("1=2")
  }

}
