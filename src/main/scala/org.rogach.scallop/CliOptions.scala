package org.rogach.scallop

import exceptions._

sealed trait CliOption {

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

  def validator: (Any) => Boolean

  def default: () => Option[Any]

  /** If true, then this option is not shown in help printout. */
  def hidden: Boolean

  /** The line, that would be printed as definition of this arg in help. */
  def argLine(sh: List[Char]): String

  /** List of argument lines, descriptions to them, and optional default values. */
  def helpInfo(sh: List[Char]): List[HelpInfo]

}

case class HelpInfo(
  argLine: String,
  description: String,
  defaultValue: () => Option[String]
)

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
    (List(sh.map("-" +), List("--" + name)).flatten.mkString(", ") + "  " + converter.argFormat(argName)).trim
  }
  def helpInfo(sh: List[Char]) = List(HelpInfo(
    argLine(sh),
    descr,
    // we don't need to remind user of default flag value, do we?
    () => (if (converter == flagConverter) None else default().map(_.toString))
  ))

  override def toString = String.format("SimpleOption(%s)", name)
}

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

  override def toString = String.format("PropertyOption(%s)", name)
}

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

  override def toString = String.format("LongNamedPropertyOption(%s)", name)
}

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

  override def toString = String.format("TrailingArgsOption(%s)", name)
}

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

  override def toString = String.format("NumberArgOption(%s)", name)
}

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

  def converter = new ValueConverter[Boolean] {
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
    HelpInfo((sh.map("-" +) ++ List("--" + name) mkString ", "), descrYes, () => None),
    HelpInfo(("--" + prefix + name), descrNo, () => None)
  )

  override def toString = String.format("ToggleOption(%s)", name)
}
