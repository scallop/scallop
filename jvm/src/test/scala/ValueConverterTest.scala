package org.rogach.scallop

import org.rogach.scallop.exceptions._

class ValueConverterTest extends ScallopTestBase {

  test ("optional value - flatMap way") {
    case class getcf(args0: Seq[String]) extends ScallopConf(args0) {
      val foo = opt[Option[String]]()(stringListConverter.flatMap {
        case Nil => Right(None)
        case v :: Nil => Right(Option(Option(v)))
        case _ => Left("wrong option format")
      })
      verify()
    }
    val conf1 = getcf(Nil)
    conf1.foo.toOption shouldBe None

    val conf2 = getcf(List("-f"))
    // bad corner case - flatMap doesn't apply when previous converter returned None
    conf2.foo.toOption shouldBe None

    val conf3 = getcf(List("-f", "bar"))
    conf3.foo.toOption shouldBe Some(Some("bar"))
  }

  /** https://github.com/Rogach/scallop/issues/57 */
  test ("issue#57: WrongOptionFormat expected and no NoSuchElementException") {
    import java.text.SimpleDateFormat
    import java.util.Date
    import org.rogach.scallop.exceptions.WrongOptionFormat

    case class getcf(args0: Seq[String]) extends ScallopConf(args0) {
      implicit def dateConverter: ValueConverter[Date] = singleArgConverter[Date](new SimpleDateFormat("yyyyMMdd").parse(_))

      val from = opt[Date]("from")
      val to = opt[Date]("to")

      validate(from, to) {
        (f, t) =>
          if (t after f) Right(())
          else Left("value of date to must be after date from")
      }

      verify()
    }

    expectException(WrongOptionFormat("from", "201305xx", "java.text.ParseException: Unparseable date: \"201305xx\"")) {
      getcf(List("-f", "201305xx", "-t", "20130515")).from()
    }
  }

  test ("optDefault - no call") {
    object Conf extends ScallopConf() {
      val apples = opt[Int]()(optDefault(5))

      verify()
    }
    Conf.apples.toOption shouldBe None
    Conf.apples.isSupplied shouldBe false
  }
  test ("optDefault - empty call") {
    object Conf extends ScallopConf(Seq("-a")) {
      val apples = opt[Int]()(optDefault(5))

      verify()
    }
    Conf.apples.toOption shouldBe Some(5)
    Conf.apples.isSupplied shouldBe true
  }
  test ("optDefault - arg provided") {
    object Conf extends ScallopConf(Seq("-a", "7")) {
      val apples = opt[Int]()(optDefault(5))

      verify()
    }
    Conf.apples.toOption shouldBe Some(7)
    Conf.apples.isSupplied shouldBe true
  }

  test ("value converter is only called once when option is retrieved multiple times") {
    var callCount = 0
    object Conf extends ScallopConf(Seq("-a", "1")) {
      val apples = opt[Int]()(singleArgConverter { str =>
        callCount += 1
        str.toInt + 2
      })
      val banans = opt[Int]()

      verify()
    }
    Conf.apples.toOption shouldBe Some(3)
    Conf.apples.toOption shouldBe Some(3)
    callCount shouldBe 1
  }

  test ("ValueConverter.map catches errors thrown from transformation function") {
    val stringToIntConverter = implicitly[ValueConverter[String]].map(_.toInt)
    expectException(WrongOptionFormat("apples", "x", "java.lang.NumberFormatException: For input string: \"x\"")) {
      object Conf extends ScallopConf(Seq("-a", "x")) {
        val apples = opt[Int]()(stringToIntConverter)
        verify()
      }
      Conf
    }
  }

  test ("ValueConverter.flatMap catches errors thrown from transformation function") {
    val stringToIntConverter = implicitly[ValueConverter[String]].flatMap(i => Right(Some(i.toInt)))
    expectException(WrongOptionFormat("apples", "x", "java.lang.NumberFormatException: For input string: \"x\"")) {
      object Conf extends ScallopConf(Seq("-a", "x")) {
        val apples = opt[Int]()(stringToIntConverter)
        verify()
      }
      Conf
    }
  }

  case class Person(name:String, phone:String)
  test ("custom converter example") {
    val personConverter = new ValueConverter[Person] {
      val nameRgx = """([A-Za-z]*)""".r
      val phoneRgx = """([0-9\-]*)""".r
      def parse(s:List[(String, List[String])]):Either[String,Option[Person]] =
        s match {
          case (_, nameRgx(name) :: phoneRgx(phone) :: Nil) :: Nil =>
            Right(Some(Person(name,phone))) // successfully found our person
          case Nil => Right(None) // no person found
          case _ => Left("wrong arguments format") // error when parsing
        }
      val argType = org.rogach.scallop.ArgType.LIST
    }
    object Conf extends ScallopConf(List("--person", "Pete", "123-45")) {
      val person = opt[Person]("person")(personConverter)
      verify()
    }
    Conf.person() shouldBe Person("Pete", "123-45")
  }

}
