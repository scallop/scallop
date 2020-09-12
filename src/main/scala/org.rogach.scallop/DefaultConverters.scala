package org.rogach.scallop

import org.rogach.scallop.exceptions.GenericScallopException

import scala.util.Try
import scala.concurrent.duration.{Duration, FiniteDuration}

trait DefaultConverters {
  implicit val flagConverter: ValueConverter[Boolean] = new ValueConverter[Boolean] {
    def parse(s: List[(String, List[String])]) = s match {
      case (_,Nil) :: Nil => Right(Some(true))
      case Nil => Right(None)
      case _ => Left("too many arguments for flag option")
    }
    val argType = ArgType.FLAG
  }

  /** Create a converter for an argument with a single value.
    * @param conv the conversion function to use, which may throw an exception on error
    * @param handler an error handler function for writing custom error messages
    */
  def singleArgConverter[A](
    conv: String => A,
    handler: PartialFunction[Throwable, Either[String, Option[A]]] = PartialFunction.empty
  ) = new ValueConverter[A] {
    def parse(s: List[(String, List[String])]) = {
      s match {
        case (_, i :: Nil) :: Nil =>
          Try(Right(Some(conv(i)))).recover(handler).recover({
            case _: Exception => Left("wrong arguments format")
          }).get
        case Nil => Right(None)
        case _ => Left("you should provide exactly one argument for this option")
      }
    }
    val argType = ArgType.SINGLE
  }

  // for some reason direct usages of singleArgConverter sometimes fail to compile under Dotty 0.27.0-RC1
  // this wrapper "fixes" the issue
  def singleArgConverter2[A](
    conv: String => A,
    handler: PartialFunction[Throwable, Either[String, Option[A]]] = PartialFunction.empty
  ) = singleArgConverter(conv, handler)

  implicit val charConverter: ValueConverter[Char] =
    singleArgConverter2[Char](_.head)
  implicit val stringConverter: ValueConverter[String] =
    singleArgConverter2[String](identity)

  /** Handler function for numeric types which expects a NumberFormatException and prints a more
    * helpful error message.
    * @param name the type name to display
    */
  def numberHandler[T](name: String): PartialFunction[Throwable, Either[String, Option[T]]] = {
    case _: NumberFormatException => Left(Util.format("bad %s value", name))
  }

  implicit val byteConverter: ValueConverter[Byte] =
    singleArgConverter[Byte](_.toByte, numberHandler("Byte"))
  implicit val shortConverter: ValueConverter[Short] =
    singleArgConverter[Short](_.toShort, numberHandler("Short"))
  implicit val intConverter: ValueConverter[Int] =
    singleArgConverter[Int](_.toInt, numberHandler("Int"))
  implicit val longConverter: ValueConverter[Long] =
    singleArgConverter[Long](_.toLong, numberHandler("Long"))
  implicit val floatConverter: ValueConverter[Float] =
    singleArgConverter[Float](_.toFloat, numberHandler("Float"))
  implicit val doubleConverter: ValueConverter[Double] =
    singleArgConverter[Double](_.toDouble, numberHandler("Double"))
  implicit val bigIntConverter: ValueConverter[BigInt] =
    singleArgConverter(BigInt(_), numberHandler("integer"))
  implicit val bigDecimalConverter: ValueConverter[BigDecimal] =
    singleArgConverter(BigDecimal(_), numberHandler("decimal"))
  implicit val durationConverter: ValueConverter[Duration] =
    singleArgConverter2(Duration(_))
  implicit val finiteDurationConverter: ValueConverter[FiniteDuration] = singleArgConverter2[FiniteDuration] { arg =>
    Duration(arg) match {
      case d: FiniteDuration => d
      case d => throw new IllegalArgumentException(s"'$d' is not a FiniteDuration.")
    }
  }

  def listArgConverter[A](conv: String => A) = new ValueConverter[List[A]] {
    def parse(s:List[(String, List[String])]) = {
      try {
        val l = s.map(_._2).flatten.map(i => conv(i))
        if (l.isEmpty) Right(None)
        else Right(Some(l))
      } catch { case _: Exception =>
        Left("wrong arguments format")
      }
    }
    val argType = ArgType.LIST
  }
  implicit val byteListConverter: ValueConverter[List[Byte]] =
    listArgConverter[Byte](_.toByte)
  implicit val shortListConverter: ValueConverter[List[Short]] =
    listArgConverter[Short](_.toShort)
  implicit val intListConverter: ValueConverter[List[Int]] =
    listArgConverter[Int](_.toInt)
  implicit val longListConverter: ValueConverter[List[Long]] =
    listArgConverter[Long](_.toLong)
  implicit val floatListConverter: ValueConverter[List[Float]] =
    listArgConverter[Float](_.toFloat)
  implicit val doubleListConverter: ValueConverter[List[Double]] =
    listArgConverter[Double](_.toDouble)
  implicit val stringListConverter: ValueConverter[List[String]] =
    listArgConverter[String](identity)

  def propsConverter[A](conv: ValueConverter[A]): ValueConverter[Map[String,A]] = new ValueConverter[Map[String,A]] {
    def parse(s:List[(String, List[String])]) = {
      try {
        Right {
          val pairs = s.map(_._2).flatten.map(_.trim).filter("," != _).flatMap(_.split("(?<!\\\\),")).map(_.replace("\\,", ","))
          val m = pairs.map { pair =>
            val kv = pair.split("(?<!\\\\)=").map(_.replace("\\=", "="))
            val key = kv(0)
            val value = kv(1)
            conv.parse(List(("",List(value)))) match {
              case Right(Some(parseResult)) => (key, parseResult)
              case Right(None) => throw new GenericScallopException("No result from props converter")
              case Left(msg) => throw new GenericScallopException(msg)
            }
          }.toMap

          if (m.nonEmpty) Some(m)
          else None
        }
      } catch { case _: Exception =>
        Left("wrong arguments format")
      }
    }
    val argType = ArgType.LIST
  }
  implicit val bytePropsConverter: ValueConverter[Map[String, Byte]] = propsConverter[Byte](byteConverter)
  implicit val shortPropsConverter: ValueConverter[Map[String, Short]] = propsConverter[Short](shortConverter)
  implicit val intPropsConverter: ValueConverter[Map[String, Int]] = propsConverter[Int](intConverter)
  implicit val longPropsConverter: ValueConverter[Map[String, Long]] = propsConverter[Long](longConverter)
  implicit val floatPropsConverter: ValueConverter[Map[String, Float]] = propsConverter[Float](floatConverter)
  implicit val doublePropsConverter: ValueConverter[Map[String, Double]] = propsConverter[Double](doubleConverter)
  implicit val charPropsConverter: ValueConverter[Map[String, Char]] = propsConverter[Char](charConverter)
  implicit val stringPropsConverter: ValueConverter[Map[String, String]] = propsConverter[String](stringConverter)

  val tallyConverter = new ValueConverter[Int] {
    def parse(s: List[(String, List[String])]) = {
      if (s.exists(_._2.nonEmpty)) Left("this option doesn't need arguments")
      else if (s.nonEmpty) Right(Some(s.size))
           else Right(None)
    }
    val argType = ArgType.FLAG
  }

  def optDefault[A](default: A)(implicit conv: ValueConverter[A]) =
    new ValueConverter[A] {
      def parse(s: List[(String, List[String])]) = {
        s match {
          case Nil => Right(None)
          case (_, Nil) :: Nil => Right(Some(default))
          case call @ ((_, v :: Nil) :: Nil) => conv.parse(call)
          case _ => Left("Too many arguments")
        }
      }
      val argType = ArgType.LIST
    }

}
