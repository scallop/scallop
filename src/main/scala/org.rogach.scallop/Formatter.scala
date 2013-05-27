package org.rogach.scallop

object Formatter {

  /** Distance between argument line description column */
  private val COLUMN_PADDING = 3
  private val DEFAULT_WIDTH = 80
  private val INDENT = 2

  /** Accepts a list of Option(argument line, option description, optional default value). If None, empty line
    * is inserted.
    * Also accepts optional width, to which the result must be formatted.
    */
  def format(s: List[Option[(String, String, Option[String])]], width: Option[Int] = None): String = {
    val neededWidth = width getOrElse DEFAULT_WIDTH
    val argWidth = if (s.isEmpty || s.head.isEmpty) 0
                   else s.map(_.map(_._1).getOrElse("")).map(a => if (a.startsWith("--")) "    " + a else a).map(_.size).max
    s.flatMap {
      case Some((arg, descr, defVal)) =>
        val argPadding = " " * (if (arg.trim.startsWith("--")) 4 else 0)
        val text = wrap(
          descr.split(" ") ++ defVal.map("(default = %s)" format _),
          neededWidth - argWidth - COLUMN_PADDING
        ).map(l => " " * (argWidth + COLUMN_PADDING + INDENT) + l)
        (" " * INDENT + argPadding + arg + text.head.drop(arg.size + argPadding.size + INDENT)) :: text.tail
      case None => // insert empty line
        List("")
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
