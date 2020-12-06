package org.rogach.scallop

import org.scalatest.funsuite.AnyFunSuite

class ValueConverterTest extends AnyFunSuite with UsefulMatchers {
  throwError.value = true

  test ("optional value - flatMap way") {
    def getcf(args: Seq[String]) = new ScallopConf(args) {
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
    import java.util.{Date, GregorianCalendar}
    import java.util.Calendar._
    import org.rogach.scallop.exceptions.WrongOptionFormat

    def d(s: String) = new SimpleDateFormat("yyyy-MM-dd").parse(s)
    def getcf(args: Seq[String]) = new ScallopConf(args) {
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

    expectException(WrongOptionFormat("from", "201305xx", "wrong arguments format")) {
      getcf(List("-f", "201305xx", "-t", "20130515")).from()
    }
  }

  test ("optDefault - no call") {
    val conf = new ScallopConf() {
      val apples = opt[Int]()(optDefault(5))

      verify()
    }
    conf.apples.toOption ==== None
    conf.apples.isSupplied ==== false
  }
  test ("optDefault - empty call") {
    val conf = new ScallopConf(Seq("-a")) {
      val apples = opt[Int]()(optDefault(5))

      verify()
    }
    conf.apples.toOption ==== Some(5)
    conf.apples.isSupplied ==== true
  }
  test ("optDefault - arg provided") {
    val conf = new ScallopConf(Seq("-a", "7")) {
      val apples = opt[Int]()(optDefault(5))

      verify()
    }
    conf.apples.toOption ==== Some(7)
    conf.apples.isSupplied ==== true
  }

  test ("value converter is only called once when option is retrieved multiple times") {
    var callCount = 0
    val conf = new ScallopConf(Seq("-a", "1")) {
      val apples = opt[Int]()(singleArgConverter { str =>
        callCount += 1
        str.toInt + 2
      })
      val banans = opt[Int]()

      verify()
    }
    conf.apples.toOption shouldBe Some(3)
    conf.apples.toOption shouldBe Some(3)
    callCount shouldBe 1
  }

}
