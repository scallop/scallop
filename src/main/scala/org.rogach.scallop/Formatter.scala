package org.rogach.scallop

object Formatter {

  /** Distance between argument line description column */
  private val COLUMN_PADDING = 3
  private val DEFAULT_WIDTH = 80
  private val INDENT = 2
  
  /** Accepts a list of (argument line, option description). 
    * Also accepts optional width, to which the result must be formatted.
    */
  def format(s: List[(String, String)], width: Option[Int] = None): String = {
    val neededWidth = width orElse getWidth getOrElse DEFAULT_WIDTH
    val argWidth =  if (s.isEmpty) 0 else s.map(_._1.size).max
    s.flatMap { case (arg, descr) => 
      val text = wrap(descr.split(" "), neededWidth - argWidth - COLUMN_PADDING).map(l => " " * (argWidth + COLUMN_PADDING + INDENT) + l)
      (" " * INDENT + arg + text.head.drop(arg.size + INDENT)) :: text.tail
    }.mkString("\n")
  }
  
  def getWidth: Option[Int] = Some(80)

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
