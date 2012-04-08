package org.rogach.scallop

object `package` {
  implicit val flagConverter = new ValueConverter[Boolean] {
    def parse(s:List[List[String]]) = s match {
      case Nil :: Nil => Right(Some(true))
      case Nil => Right(Some(false))
      case _ => Left(Unit)
    }
    val manifest = implicitly[Manifest[Boolean]]
    val argType = ArgType.FLAG
  }
  
  def singleArgConverter[A](conv: String => A)(implicit m:Manifest[A]) = new ValueConverter[A] {
    def parse(s:List[List[String]]) = {
      s match {
        case (i :: Nil) :: Nil => 
          try { Right(Some(conv(i))) } catch { case _ => Left(Unit) }
        case Nil => Right(None)
        case _ => Left(Unit)
      }
    }
    val manifest = m
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
  
  def listArgConverter[A](conv: String => A)(implicit m:Manifest[List[A]])  = new ValueConverter[List[A]] {
    def parse(s:List[List[String]]) = {
      try {
        val l = s.flatten.map(i => conv(i))
        if (l.isEmpty) Right(None)
        else Right(Some(l))
      } catch { case _ =>
        Left(Unit)
      }
    }
    val manifest = m
    val argType = ArgType.LIST
  }
  implicit val byteListConverter = listArgConverter[Byte](_.toByte)
  implicit val shortListConverter = listArgConverter[Short](_.toShort)
  implicit val intListConverter = listArgConverter[Int](_.toInt)
  implicit val longListConverter = listArgConverter[Long](_.toLong)
  implicit val floatListConverter = listArgConverter[Float](_.toFloat)
  implicit val doubleListConverter = listArgConverter[Double](_.toDouble)
  implicit val stringListConverter = listArgConverter[String](a => a)
  
  implicit val propsConverter = listArgConverter[(String,String)] { a =>
    val rgx = """([^=]+)=(.*)""".r
    a match {
      case rgx(key,value) => (key,value)
    }
  }.map(_.toMap)

}
