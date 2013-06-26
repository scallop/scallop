package org.rogach

import reflect.runtime.universe._
import java.io.File

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

  def singleArgConverter[A](conv: String => A)(implicit tt: TypeTag[A]) = new ValueConverter[A] {
    def parse(s: List[(String, List[String])]) = {
      s match {
        case (_, i :: Nil) :: Nil =>
          try { Right(Some(conv(i))) } catch { case _: Throwable => Left("wrong arguments format") }
        case Nil => Right(None)
        case _ => Left("you should provide exactly one argument for this option")
      }
    }
    val tag = tt
    val argType = ArgType.SINGLE
  }
  implicit val byteConverter = singleArgConverter[Byte](_.toByte)
  implicit val shortConverter = singleArgConverter[Short](_.toShort)
  implicit val intConverter = singleArgConverter[Int](_.toInt)
  implicit val longConverter = singleArgConverter[Long](_.toLong)
  implicit val floatConverter = singleArgConverter[Float](_.toFloat)
  implicit val doubleConverter = singleArgConverter[Double](_.toDouble)
  implicit val charConverter = singleArgConverter[Char](_.head)
  implicit val stringConverter = singleArgConverter[String](a=>a)
  implicit val fileConverter = stringConverter.map(new File(_)).flatMap { f =>
    if (f.exists) Right(Some(f)) else Left("file '%s' doesn't exist" format f)
  }

  def listArgConverter[A](conv: String => A)(implicit tt: TypeTag[List[A]])  = new ValueConverter[List[A]] {
    def parse(s:List[(String, List[String])]) = {
      try {
        val l = s.map(_._2).flatten.map(i => conv(i))
        if (l.isEmpty) Right(None)
        else Right(Some(l))
      } catch { case _: Throwable =>
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
  implicit val stringListConverter = listArgConverter[String](a => a)

  def propsConverter[A](conv: ValueConverter[A])(implicit tt: TypeTag[Map[String,A]]): ValueConverter[Map[String,A]] = new ValueConverter[Map[String,A]] {
    val rgx = """([^=]+)=(.*)""".r
    def parse(s:List[(String, List[String])]) = {
      try {
        Right(Some(s.map(_._2).flatten.map(_.trim).filter(","!=).flatMap(_ split "," filter (_.trim.size > 0)).map {
          case rgx(key,value) => (key, conv.parse(List(("",List(value)))).right.get.get)
        }.toMap))
      } catch { case _: Throwable =>
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

}
