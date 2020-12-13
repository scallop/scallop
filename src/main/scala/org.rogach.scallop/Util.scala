package org.rogach.scallop

object Util {
  /** Use instead of java.lang.String.format, because it is not supported on Scala Native */
  def format(formatString: String, args: Any*): String = {
    val s = new StringBuilder
    var si = 0
    var ai = 0
    while (si < formatString.length) {
      val c = formatString.charAt(si)
      if (c == '%') {
        if (si + 1 < formatString.length) {
          val f = formatString.charAt(si + 1)
          if (f >= '0' && f <= '9') {
            if (si + 3 < formatString.length) {
              val i = f.toString.toInt - 1
              formatString.charAt(si + 3) match {
                case 's' =>
                  s.append(String.valueOf(args(i)))
                case 'd' =>
                  s.append(String.valueOf(args(i)))
                case _ =>
                  new java.util.MissingFormatArgumentException(formatString.substring(si, 4))
              }
            } else {
              throw new java.util.UnknownFormatConversionException("Conversion = '" + formatString.substring(si) + "'")
            }
            si += 4
          } else {
            f match {
              case 's' =>
                s.append(String.valueOf(args(ai)))
              case 'd' =>
                s.append(String.valueOf(args(ai)))
              case _ =>
                new java.util.MissingFormatArgumentException("%" + f)
            }
            ai += 1
            si += 2
          }
        } else {
          throw new java.util.UnknownFormatConversionException("Conversion = '%'")
        }
      } else {
        s.append(c)
        si += 1
      }
    }
    s.toString
  }

  def seqstr(items: Seq[Any]): String =
    items.mkString("'", "', '", "'")
}
