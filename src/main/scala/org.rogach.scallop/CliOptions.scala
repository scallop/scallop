package org.rogach.scallop

import exceptions._

/** Parent class for option descriptors.
  * Each option descriptor contains everything needed to parse that option - option names, defaults, converters, validators, etc. */
sealed trait CliOption {

  /** Long names for this option. */
  def longNames: List[String]

  /** Short names that were explicitly set for this option. */
  def requiredShortNames: List[Char]

  /** Short names that are suggested by option implementation. */
  def shortNames: List[Char]

  /** True for trailing argument option type, false for everything else. */
  def isPositional: Boolean

  /** Converter for pure string arguments to the needed type of this option. */
  def converter: ValueConverter[_]

  /** Internal name of this option - the one that would be used to access parsed values. */
  def name: String

  /** Description for this option that will be presented to the user */
  def descr: String

  /** Is there a requirement to have at least one invocation of this option? */
  def required: Boolean

  /** Validator for the option value. */
  def validator: (Any) => Boolean

  /** Function that provides an optional default value for this option. */
  def default: () => Option[Any]

  /** If true, then this option is not shown in help output. */
  def hidden: Boolean

  /** The line that would be printed as definition of this arg in help output. */
  def argLine(sh: List[Char]): String

  /** List of argument lines, descriptions to them, and optional default values. */
  def helpInfo(sh: List[Char]): List[HelpInfo]

}

/** Internal class - container for help information for a single option. */
case class HelpInfo(
  argLine: String,
  description: String,
  defaultValue: () => Option[String]
)

/** Descriptor for a simple option - describes flag, one-arg or multi-arg options (--opt [ARGS]...).
  * @param name Name for new option, used as long option name in parsing, and for option identification.
  * @param short Overload the char that will be used as short option name.
  * @param descr Description for this option, for help output.
  * @param required Is this option required?
  * @param converter The converter for this option.
  * @param default Default value to use if option is not found in input arguments.
  * @param validator The function that validates the parsed value.
  * @param argName The name for this option argument, as it will appear in help.
  * @param hidden Hides description of this option from help (this can be useful for debugging options).
  * @param noshort If set to true, then this option does not have any short name.
  */
case class SimpleOption(
  name: String,
  short: Option[Char],
  descr: String,
  required: Boolean,
  converter: ValueConverter[_],
  default: () => Option[Any],
  validator: (Any) => Boolean,
  argName: String,
  hidden: Boolean,
  noshort: Boolean
) extends CliOption {

  def isPositional = false
  def longNames = List(name)
  def shortNames = if (noshort) Nil else List(short.getOrElse(name.head))
  def requiredShortNames = if (noshort) Nil else short.toList

  def argLine(sh: List[Char]): String = {
    (List(sh.map("-" + _), List("--" + name)).flatten.mkString(", ") + "  " + converter.argFormat(argName)).trim
  }
  def helpInfo(sh: List[Char]) = List(HelpInfo(
    argLine(sh),
    descr,
    // we don't need to remind user of default flag value, do we?
    () => (if (converter == flagConverter) None else default().map(_.toString))
  ))

  override def toString = Util.format("SimpleOption(%s)", name)
}

/** Descriptor for a property option (like `-Dkey=value` or `-D key1=value1 key2=value2`).
  * @param name Internal name for the option.
  * @param short Character that will be used as prefix for property arguments.
  * @param descr Description for this property option, for help description.
  * @param converter The converter for this option.
  * @param keyName Name for 'key' part of this option arg name, as it will appear in help option definition.
  * @param valueName Name for 'value' part of this option arg name, as it will appear in help option definition.
  * @param hidden If set to true, then this option will be hidden from generated help output.
  */
case class PropertyOption(
  name: String,
  short: Char,
  descr: String,
  converter: ValueConverter[_],
  keyName: String,
  valueName: String,
  hidden: Boolean
) extends CliOption {

  def isPositional = false
  def longNames = Nil
  def shortNames = List(short)
  def requiredShortNames = shortNames
  def validator = (a) => true
  def default = () => Some(Map())
  def required = false

  def argLine(sh: List[Char]): String =
    Util.format("-%1$s%2$s=%3$s [%2$s=%3$s]...", short, keyName, valueName)

  def helpInfo(sh: List[Char]) = List(HelpInfo(argLine(sh), descr, () => None))

  override def toString = Util.format("PropertyOption(%s)", name)
}

/** Descriptor for a property option with a "long" name (like `--Props key1=value1 key2=value2`).
  * @param name Internal name for the option.
  * @param descr Description for this property option, for help description.
  * @param converter The converter for this option.
  * @param keyName Name for 'key' part of this option arg name, as it will appear in help option definition.
  * @param valueName Name for 'value' part of this option arg name, as it will appear in help option definition.
  * @param hidden If set to true, then this option will be hidden from generated help output.
  */
case class LongNamedPropertyOption(
  name: String,
  descr: String,
  converter: ValueConverter[_],
  keyName: String,
  valueName: String,
  hidden: Boolean
) extends CliOption {

  def isPositional = false
  def longNames = List(name)
  def shortNames = Nil
  def requiredShortNames = Nil
  def validator = (a) => true
  def default = () => Some(Map())
  def required = false

  def argLine(sh: List[Char]) =
    Util.format("--%1$s %2$s=%3$s [%2$s=%3$s]...", name, keyName, valueName)

  def helpInfo(sh: List[Char]) = List(HelpInfo(argLine(sh), descr, () => None))

  override def toString = Util.format("LongNamedPropertyOption(%s)", name)
}

/** Descriptor for a trailing arg option.
  * @param name Name for new definition, used for identification.
  * @param required Is this trailing argument required?
  * @param descr Description for this option, for help text.
  * @param converter The converter for this option.
  * @param validator The function that validates the parsed value.
  * @param default If this argument is not required and not found in the argument list, use this value.
  * @param hidden If set to true then this option will not be present in auto-generated help.
  */
case class TrailingArgsOption(
  name: String,
  required: Boolean,
  descr: String,
  converter: ValueConverter[_],
  validator: (Any) => Boolean,
  default: () => Option[Any],
  hidden: Boolean
) extends CliOption {

  def isPositional = true
  def longNames = Nil
  def shortNames = Nil
  def requiredShortNames = Nil

  def argLine(sh: List[Char]): String =
    Util.format("%s (%s)", name, (if (required) "required" else "not required"))

  def helpInfo(sh: List[Char]) = List(HelpInfo(argLine(sh), descr, () => default().map(_.toString)))

  override def toString = Util.format("TrailingArgsOption(%s)", name)
}

/** Descriptor for a number option (`-1` or `-3`, like GNU tail for example).
  * @param name Name for new definition, used for identification.
  * @param required Is this trailing argument required?
  * @param descr Description for this option, for help text.
  * @param converter The converter for this option.
  * @param validator The function that validates the parsed value.
  * @param default If this argument is not required and not found in the argument list, use this value.
  * @param hidden If set to true then this option will not be present in auto-generated help.
  */
case class NumberArgOption(
  name: String,
  required: Boolean,
  descr: String,
  converter: ValueConverter[Long],
  validator: (Any) => Boolean,
  default: () => Option[Long],
  hidden: Boolean
) extends CliOption {

  def isPositional = false
  def longNames = Nil
  def shortNames = Nil
  def requiredShortNames = Nil

  def argLine(sh: List[Char]): String =
    Util.format("-<%s>", name)

  def helpInfo(sh: List[Char]) = List(HelpInfo(argLine(sh), descr, () => default().map(_.toString)))

  override def toString = Util.format("NumberArgOption(%s)", name)
}

/** Descriptor for a toggle option (like `--verbose/--noverbose`).
  * @param name Name of this option.
  * @param default Default value for this option.
  * @param short Overload the char that will be used as short option name.
  * @param noshort If set to true, then this option will not have any short name.
  * @param prefix Prefix to name of the option, that will be used for "negative" version of the option.
  * @param descrYes Description for positive variant of this option.
  * @param descrNo Description for negative variant of this option.
  * @param hidden If set to true, then this option will not be present in auto-generated help.
  */
case class ToggleOption(
  name: String,
  default: () => Option[Boolean],
  short: Option[Char],
  noshort: Boolean,
  prefix: String,
  descrYes: String,
  descrNo: String,
  hidden: Boolean
) extends CliOption {

  def descr = ""
  def isPositional = false
  def validator = (a) => true
  def required = false

  def shortNames = if (noshort) Nil else List(short.getOrElse(name.head))
  def requiredShortNames = if (noshort) Nil else short.toList
  def longNames = List(name, prefix + name)

  def converter: ValueConverter[Boolean] = new ValueConverter[Boolean] {
    def parse(s: List[(String, List[String])]) = {
      val noname = prefix + name
      val shortname = short.getOrElse(name.head).toString
      s match {
        case (`name`, Nil) :: Nil => Right(Some(true))
        case (`noname`, Nil) :: Nil => Right(Some(false))
        case (`shortname`, Nil) :: Nil => Right(Some(true))
        case Nil => Right(None)
        case ("", Nil) :: Nil => Right(Some(true)) // it is called this way only when parsing trailing args
                                                   // but such hack may cause some problems in the future
        case _ => Left("wrong arguments format")
      }
    }
    val argType = ArgType.FLAG
  }

  def argLine(sh: List[Char]): String = throw new MajorInternalException
  def helpInfo(sh: List[Char]) = List(
    HelpInfo((sh.map("-" + _) ++ List("--" + name) mkString ", "), descrYes, () => None),
    HelpInfo(("--" + prefix + name), descrNo, () => None)
  )

  override def toString = Util.format("ToggleOption(%s)", name)
}
