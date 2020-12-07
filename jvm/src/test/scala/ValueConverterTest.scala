package org.rogach.scallop

import org.scalatest.funsuite.AnyFunSuite
import org.rogach.scallop.exceptions._

class ValueConverterTest extends AnyFunSuite with UsefulMatchers {
  throwError.value = true

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
    conf1.foo.toOption ==== None

    val conf2 = getcf(List("-f"))
    // bad corner case - flatMap doesn't apply when previous converter returned None
    conf2.foo.toOption ==== None

    val conf3 = getcf(List("-f", "bar"))
    conf3.foo.toOption ==== Some(Some("bar"))
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
    Conf.apples.toOption ==== None
    Conf.apples.isSupplied ==== false
  }
  test ("optDefault - empty call") {
    object Conf extends ScallopConf(Seq("-a")) {
      val apples = opt[Int]()(optDefault(5))

      verify()
    }
    Conf.apples.toOption ==== Some(5)
    Conf.apples.isSupplied ==== true
  }
  test ("optDefault - arg provided") {
    object Conf extends ScallopConf(Seq("-a", "7")) {
      val apples = opt[Int]()(optDefault(5))

      verify()
    }
    Conf.apples.toOption ==== Some(7)
    Conf.apples.isSupplied ==== true
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

}
