package org.rogach

import reflect.runtime.universe._
import java.io.File
import java.net.{MalformedURLException, URL, URI, URISyntaxException}
import java.nio.file.{InvalidPathException,Path,Paths}
import org.rogach.scallop.exceptions.GenericScallopException

import scala.util.Try

package object scallop {
  implicit val flagConverter = new ValueConverter[Boolean] {
    def parse(s: List[(String, List[String])]) = s match {
      case (_,Nil) :: Nil => Right(Some(true))
      case Nil => Right(None)
      case _ => Left("too many arguments for flag option")
    }
    val tag = typeTag[Boolean]
    val argType = ArgType.FLAG
  }

  /** Create a converter for an argument with a single value.
    * @param conv the conversion function to use, which may throw an exception on error
    * @param handler an error handler function for writing custom error messages
    */
  def singleArgConverter[A](
    conv: String => A, handler: PartialFunction[Throwable, Either[String, Option[A]]] = PartialFunction.empty
  )(implicit tt: TypeTag[A]) = new ValueConverter[A] {
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
    val tag = tt
    val argType = ArgType.SINGLE
  }

  implicit val charConverter = singleArgConverter[Char](_.head)
  implicit val stringConverter = singleArgConverter[String](identity)

  /** Handler function for numeric types which expects a NumberFormatException and prints a more
    * helpful error message.
    * @param name the type name to display
    */
  def numberHandler[T](name: String): PartialFunction[Throwable, Either[String, Option[T]]] = {
    case _: NumberFormatException => Left("bad %s value" format name)
  }

  implicit val byteConverter = singleArgConverter[Byte](_.toByte, numberHandler("Byte"))
  implicit val shortConverter = singleArgConverter[Short](_.toShort, numberHandler("Short"))
  implicit val intConverter = singleArgConverter[Int](_.toInt, numberHandler("Int"))
  implicit val longConverter = singleArgConverter[Long](_.toLong, numberHandler("Long"))
  implicit val floatConverter = singleArgConverter[Float](_.toFloat, numberHandler("Float"))
  implicit val doubleConverter = singleArgConverter[Double](_.toDouble, numberHandler("Double"))
  implicit val bigIntConverter = singleArgConverter(BigInt(_), numberHandler("integer"))
  implicit val bigDecimalConverter = singleArgConverter(BigDecimal(_), numberHandler("decimal"))
  implicit val fileConverter = singleArgConverter(new File(_))
  implicit val pathConverter = singleArgConverter[Path](Paths.get(_), {
    case e: InvalidPathException => Left("bad Path, %s" format e.getMessage)
  })
  implicit val urlConverter = singleArgConverter(new URL(_), {
    case e: MalformedURLException => Left("bad URL, %s" format e.getMessage)
  })
  implicit val uriConverter = singleArgConverter(new URI(_), {
    case e: URISyntaxException => Left("bad URI, %s" format e.getMessage)
  })

  def listArgConverter[A](conv: String => A)(implicit tt: TypeTag[List[A]])  = new ValueConverter[List[A]] {
    def parse(s:List[(String, List[String])]) = {
      try {
        val l = s.map(_._2).flatten.map(i => conv(i))
        if (l.isEmpty) Right(None)
        else Right(Some(l))
      } catch { case _: Exception =>
        Left("wrong arguments format")
      }
    }
    val tag = tt
    val argType = ArgType.LIST
  }
  implicit val byteListConverter = listArgConverter[Byte](_.toByte)
  implicit val shortListConverter = listArgConverter[Short](_.toShort)
  implicit val intListConverter = listArgConverter[Int](_.toInt)
  implicit val longListConverter = listArgConverter[Long](_.toLong)
  implicit val floatListConverter = listArgConverter[Float](_.toFloat)
  implicit val doubleListConverter = listArgConverter[Double](_.toDouble)
  implicit val stringListConverter = listArgConverter[String](identity)

  def propsConverter[A](conv: ValueConverter[A])(implicit tt: TypeTag[Map[String,A]]): ValueConverter[Map[String,A]] = new ValueConverter[Map[String,A]] {
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
    val tag = tt
    val argType = ArgType.LIST
  }
  implicit val bytePropsConverter = propsConverter[Byte](byteConverter)
  implicit val shortPropsConverter = propsConverter[Short](shortConverter)
  implicit val intPropsConverter = propsConverter[Int](intConverter)
  implicit val longPropsConverter = propsConverter[Long](longConverter)
  implicit val floatPropsConverter = propsConverter[Float](floatConverter)
  implicit val doublePropsConverter = propsConverter[Double](doubleConverter)
  implicit val charPropsConverter = propsConverter[Char](charConverter)
  implicit val stringPropsConverter = propsConverter[String](stringConverter)

  val tallyConverter = new ValueConverter[Int] {
    def parse(s: List[(String, List[String])]) = {
      if (s.exists(_._2.nonEmpty)) Left("this option doesn't need arguments")
      else if (s.nonEmpty) Right(Some(s.size))
           else Right(None)
    }
    val tag = implicitly[TypeTag[Int]]
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
      val tag = conv.tag
      val argType = ArgType.LIST
    }

}
