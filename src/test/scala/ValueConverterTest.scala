package org.rogach.scallop

import org.scalatest.FunSuite
import org.rogach.scallop._
import org.rogach.scallop.exceptions._
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

}
