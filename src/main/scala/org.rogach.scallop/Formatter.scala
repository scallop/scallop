package org.rogach.scallop

object Formatter {

  /** Distance between option name and description column */
  private val ColumnPadding = 3
  private val DefaultWidth = 80
  private val Indent = 2

  /** Accepts a list of Option(argument line, option description, optional default value). If None, empty line
    * is inserted.
    * Also accepts optional width, to which the result must be formatted.
    */
  def format(s: List[Either[String, HelpInfo]], width: Option[Int], appendDefault: Boolean): String = {
    val neededWidth = width.getOrElse(DefaultWidth)
    val helpInfos = s.flatMap {
      case Left(_) => None
      case Right(helpInfo) => Some(helpInfo)
    }
    val argWidth =
      if (helpInfos.isEmpty) 0
      else helpInfos.map(_.argLine).map(a => if (a.startsWith("--")) "    " + a else a).map(_.size).max
    s.flatMap {
      case Left(s) => // insert line as-is
        List(s)
      case Right(HelpInfo(arg, descr, defVal)) =>
        val argPadding = " " * (if (arg.trim.startsWith("--")) 4 else 0)
        val splitDescr = (if (descr.nonEmpty) descr.split("\n").toList else Nil).map(_.split(" ").toList)
        val dfltList = if (appendDefault) defVal().map(v => Util.format("(default = %s)", v)) else None
        val joinedDescrs =
          splitDescr match {
            case head :: Nil => // If there is a single line, attempt to place dflt at the end of the line
              (head ++ dfltList) :: Nil
            case _ => // Otherwise, put dflt on its own line
              splitDescr ++ dfltList.map(_ :: Nil)
          }
        val totalIndent = argWidth + ColumnPadding + Indent
        val text =
          joinedDescrs
            .flatMap(wrap(_, neededWidth - totalIndent))
            .map(l => " " * totalIndent + l)
        val argStr = " " * Indent + argPadding + arg
        text match {
          case head :: tail => (argStr + head.drop(arg.size + argPadding.size + Indent)) :: tail
          case Nil => argStr :: Nil
        }
    }.mkString("\n")
  }

  /** Carefully wraps the text to the needed width. */
  def wrap(s: Seq[String], width: Int): List[String] = {
    var text = List[String]("")
    s foreach { w =>
      if (text.last.size + 1 + w.size <= width) {
        text = text.init :+ (text.last + w + " ")
      } else if (text.last.size + w.size <= width) {
        text = text.init :+ (text.last + w)
      } else text :+= (w + " ")
    }
    text
  }

}
