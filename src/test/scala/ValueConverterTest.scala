package org.rogach.scallop

import org.scalatest.FunSuite
import scala.reflect.runtime.universe.TypeTag

class ValueConverterTest extends FunSuite {
  throwError.value = true

  test ("optional value - flatMap way") {
    implicit def optConv[A](implicit tt: TypeTag[Option[A]], conv: ValueConverter[List[A]]) =
      conv.flatMap {
        case Nil => Right(None)
        case v :: Nil => Right(Option(Option(v)))
        case _ => Left(Unit)
      }
    def getcf(args: Seq[String]) = new ScallopConf(args) {
      val foo = opt[Option[String]]()
    }
    val conf1 = getcf(Nil)
    conf1.foo.get === None
    val conf2 = getcf(List("-f"))
    conf2.foo.get === Some(None)
    val conf3 = getcf(List("-f", "bar"))
    conf3.foo.get === Some(Some("bar"))
  }

  /** https://github.com/Rogach/scallop/issues/57 */
  ignore("issue#57: WrongOptionFormat expected and no NoSuchElementException") {
    import java.text.SimpleDateFormat
    import java.util.{Date, GregorianCalendar}
    import java.util.Calendar._
    import org.rogach.scallop.exceptions.WrongOptionFormat

    def d(s: String) = new SimpleDateFormat("yyyy-MM-dd").parse(s)
    def getcf(args: Seq[String]) = new ScallopConf(args) {
      implicit def dateConverter: ValueConverter[Date] = singleArgConverter[Date](new SimpleDateFormat("yyyyMMdd").parse(_))

      def previousMonth(d: Date): Date = {
        val gc = new GregorianCalendar()
        gc.setTime(d)
        gc.add(MONTH, -1)
        gc.getTime
      }

      val from = opt[Date]("from")
      val to = opt[Date]("to")

      validate(from, to) {
        (f, t) =>
          if (t after f) Right(Unit)
          else Left("value of date to must be after date from")
      }

    }

    assert(getcf(List("-f", "20130514", "-t", "20130515")).from() === d("2013-05-14"))

    intercept[WrongOptionFormat] {
      getcf(List("-f", "201305xx", "-t", "20130515")).from()
    }
  }

}
