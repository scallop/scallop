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
        val text = wrap(
          descr.split(" ").toSeq ++
            (if (appendDefault) defVal().map(v => Util.format("(default = %s)", v)).toList else Nil),
          neededWidth - argWidth - ColumnPadding - Indent
        ).map(l => " " * (argWidth + ColumnPadding + Indent) + l)
        (" " * Indent + argPadding + arg + text.head.drop(arg.size + argPadding.size + Indent)) :: text.tail
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
