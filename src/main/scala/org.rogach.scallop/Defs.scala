package org.rogach.scallop

/** Parent class for option and property definitions. 
  *
  * @param _name Name for this arg.
  * @param _conv Converter for this arg, holding manifest as well.
  */
abstract class ArgDef(
    val _name: String,
    val _short: Option[Char],
    val _descr: String,
    val _conv: ValueConverter[_],
    val _hidden: Boolean) {
      
  /** The line, that would be printed as definition of this arg in help. */
  def argLine(sh: Option[Char]): String
  
  /** The full text of definition+description for this arg, as it will appear in options help. */
  def help(sh: Option[Char]): String = {
    if (!_hidden) {
      var text = List[String]("")
      _descr.split(" ") foreach { w =>
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
/** Holder for option definition.
  *
  * @param name Name for this option, used for identification.
  * @param short Character for short name of this option, defaults to first character of option's name.
  * @param descr Description for this option, used in help printing.
  * @param conv Converter for this options arguments.
  * @param default Default value for this option. Defaults to None.
  * @param required Is this option required? Defaults to false. 
  * @param noshort Explicitly supresses any short name of this option.
  * @param arg The name for this option argument in help definition.
  */
case class OptDef (
    name: String,
    short: Option[Char],
    descr: String,
    conv: ValueConverter[_], 
    default: Option[Any],
    validator: (Manifest[_],Any)=>Boolean,
    required: Boolean,
    arg: String,
    hidden: Boolean,
    noshort: Boolean)
  extends ArgDef(name, short, descr, conv, hidden) {

  def argLine(sh: Option[Char]): String =
    List(sh.map("-" +), Some("--" + name)).flatten.mkString(", ") + "  " + conv.argType.fn(arg)
}
/** Holder for property option definition.
  *
  * @param char The char that is used as prefix for property options, and for identification.
  * @param descr Description for this option, used in help printing.
  * @param keyName Name for the 'key' part of option argument, as will be printed in help description.
  * @param valueName Name for the 'value' part of option argument, as will be printed in help description.
  */
case class PropDef (
    char: Char,
    descr: String,
    conv: ValueConverter[_],
    keyName: String,
    valueName: String,
    hidden: Boolean)
  extends ArgDef(char.toString, Some(char), descr, conv, hidden) {

  def argLine(sh: Option[Char]) =
    "-%1$s%2$s=%3$s [%2$s=%3$s]..." format (char, keyName, valueName)
}
/** Holder for trail argument definition.
  *
  * @param name Name for this definition, used for identification.
  * @param required Is this arg required?
  * @param conv Converter for this argument.
  */
case class TrailDef (
  name: String,
  required: Boolean,
  conv: ValueConverter[_],
  default: Option[Any])
