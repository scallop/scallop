package org.rogach.scallop

import exceptions._

trait CliOption {
  
  def longNames: List[String]

  /** Names, that were explicitly requested by the user */
  def requiredShortNames: List[Char]

  /** Short names, that are suggested by option implementation, not the user */
  def shortNames: List[Char]
  
  def isPositional: Boolean
  
  /** Converter for pure string arguments to the needed type of this option.
   */
  def converter: ValueConverter[_]
  
  /** "Internal" name of this option - the one that
   *  would be used to access parsed values.
   */
  def name: String
  
  /** Description for this option, that will be presented to the user */
  def descr: String
  
  /** Is there a requirement to have at least one invocation of this option? */
  def required: Boolean
  
  def validator: (Manifest[_], Any) => Boolean
  
  def default: Option[Any]
  
  /** If true, then this option is not shown in help printout. */
  def hidden: Boolean
  
  /** The line, that would be printed as definition of this arg in help. */
  def argLine(sh: List[Char]): String
  
  /** The full text of definition+description for this arg, as it will appear in options help. */
  def help(sh: List[Char]): List[String] = {
    if (!hidden) {
      var text = format(descr.split(" "))
      List((argLine(sh) + "\n" + text.map("    " +).mkString("\n")).trim)
    } else Nil
  }
  
  protected def format(s: Seq[String]): List[String] = {
    var text = List[String]("")
    s foreach { w =>
      if (text.last.size + 1 + w.size <= 76) {
        text = text.init :+ (text.last + w + " ")
      } else if (text.last.size + w.size <= 76) {
        text = text.init :+ (text.last + w)
      } else text :+= w
    }
    text
  }
}

case class SimpleOption(
    name: String,
    short: Option[Char],
    descr: String,
    required: Boolean,
    converter: ValueConverter[_],
    default: Option[Any],
    validator: (Manifest[_], Any) => Boolean,
    argName: String,
    hidden: Boolean,
    noshort: Boolean) 
  extends CliOption {

  def isPositional = false
  def longNames = List(name)
  def shortNames = if (noshort) Nil else List(short.getOrElse(name.head))
  def requiredShortNames = if (noshort) Nil else short.toList

  def argLine(sh: List[Char]): String = {
    List(sh.map("-" +), List("--" + name)).flatten.mkString(", ") + "  " + converter.argType.fn(argName)    
  }
}

case class PropertyOption(
    name: String,
    short: Char,
    descr: String,
    converter: ValueConverter[_],
    keyName: String,
    valueName: String,
    hidden: Boolean)
  extends CliOption {

  def isPositional = false
  def longNames = Nil
  def shortNames = List(short)
  def requiredShortNames = shortNames
  def validator = (a,b) => true
  def default = Some(Map())
  def required = false
  
  def argLine(sh: List[Char]): String =
    "-%1$s%2$s=%3$s [%2$s=%3$s]..." format (short, keyName, valueName)

}

case class LongNamedPropertyOption(
    name: String,
    descr: String,
    converter: ValueConverter[_],
    keyName: String,
    valueName: String,
    hidden: Boolean)
  extends CliOption {

  def isPositional = false
  def longNames = List(name)
  def shortNames = Nil
  def requiredShortNames = Nil
  def validator = (a,b) => true
  def default = Some(Map())
  def required = false

  def argLine(sh: List[Char]) =
    "--%1$s%2$s=%3$s [%2$s=%3$s]..." format (name, keyName, valueName)
  
}

case class TrailingArgsOption(
    name: String,
    required: Boolean,
    descr: String,
    converter: ValueConverter[_],
    validator: (Manifest[_],Any) => Boolean,
    default: Option[Any],
    hidden: Boolean)
  extends CliOption {

  def isPositional = true
  def longNames = Nil
  def shortNames = Nil
  def requiredShortNames = Nil

  def argLine(sh: List[Char]): String = 
    "%s (%s)" format (name, (if (required) "required" else "not required"))
}

case class ToggleOption(
    name: String,
    default: Option[Boolean],
    short: Option[Char],
    noshort: Boolean,
    prefix: String,
    descrYes: String,
    descrNo: String,
    hidden: Boolean)
  extends CliOption {

  def descr = ""
  def isPositional = false
  def validator = (a,b) => true
  def required = false

  def shortNames = if (noshort) Nil else List(short.getOrElse(name.head))
  def requiredShortNames = if (noshort) Nil else short.toList
  def longNames = List(name, prefix + name)
  
  def converter = new ValueConverter[Boolean] {
    def parse(s: List[(String, List[String])]) = {
      val noname = prefix + name
      val shortname = name.head.toString
      s match {
        case (`name`, Nil) :: Nil => Right(Some(true))
        case (`noname`, Nil) :: Nil => Right(Some(false))
        case (`shortname`, Nil) :: Nil => Right(Some(true))
        case Nil => Right(None)
        case ("", Nil) :: Nil => Right(Some(true)) // it is called this way only when parsing trailing args
                                                   // but such hack may cause some problems in the future
        case _ => Left(Unit)
      }
    }
    val manifest = implicitly[Manifest[Boolean]]
    val argType = ArgType.FLAG
  }
  
  def argLine(sh: List[Char]): String = throw new MajorInternalException
  override def help(sh: List[Char]): List[String] = {
    List(
      (sh.map("-" +) ++ List("--" + name) mkString ", ") + "\n" + 
      format(descrYes.split(" ")).map("    " +).mkString("\n"),
      
      ("--" + prefix + name) + "\n" +
      format(descrNo.split(" ")).map("    " +).mkString("\n")
    )
  }
  
}
  
