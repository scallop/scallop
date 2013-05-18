package org.rogach.scallop

import org.scalatest.FunSuite

class ValueConverterTest extends FunSuite with UsefulMatchers {
  throwError.value = true

  test ("optional value - flatMap way") {
    implicit def optConv[A](implicit m: Manifest[Option[A]], conv: ValueConverter[List[A]]) =
      conv.flatMap {
        case Nil => Right(None)
        case v :: Nil => Right(Option(Option(v)))
        case _ => Left(Unit)
      }
    def getcf(args: Seq[String]) = new ScallopConf(args) {
      val foo = opt[Option[String]]()
    }
    val conf1 = getcf(Nil)
    conf1.foo.get ==== None

    val conf2 = getcf(List("-f"))
    // bad corner case - flatMap doesn't apply when previous converter returned None
    conf2.foo.get ==== None

    val conf3 = getcf(List("-f", "bar"))
    conf3.foo.get ==== Some(Some("bar"))
  }

  /** https://github.com/Rogach/scallop/issues/57 */
  test("issue#57: WrongOptionFormat expected and no NoSuchElementException") {
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
          if (t after f) Right(Unit)
          else Left("value of date to must be after date from")
      }

    }

    intercept[WrongOptionFormat] {
      getcf(List("-f", "201305xx", "-t", "20130515")).from()
    }
  }

}
