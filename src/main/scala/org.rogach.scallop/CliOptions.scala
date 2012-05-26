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
  def help(sh: List[Char]): String = {
    if (!hidden) {
      var text = List[String]("")
      descr.split(" ") foreach { w =>
        if (text.last.size + 1 + w.size <= 76) {
          text = text.init :+ (text.last + w + " ")
        } else if (text.last.size + w.size <= 76) {
          text = text.init :+ (text.last + w)
        } else text :+= w
      }
      (argLine(sh) + "\n" + text.map("    " +).mkString("\n")).trim
    } else ""
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
    converter: ValueConverter[_],
    validator: (Manifest[_],Any) => Boolean,
    default: Option[Any])
  extends CliOption {
    
  def descr = ""
  def isPositional = true
  def longNames = Nil
  def shortNames = Nil
  def requiredShortNames = Nil

  def hidden = true
  def argLine(sh: List[Char]): String = 
    throw new UnsupportedOperationException("This method is not imptemented yet")
}
