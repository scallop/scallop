package org.rogach.scallop

import java.io.File
import java.nio.file.{Files, Path}
import scala.collection.{Seq => CSeq}
import scala.collection.mutable.ArrayBuffer

import org.rogach.scallop.exceptions._

/** Base class for CLI subcommands. */
class Subcommand(commandNameAndAliases: String*) extends ScallopConf(Nil, commandNameAndAliases) {
  /** Short description for this subcommand. Used if parent command has shortSubcommandsHelp enabled. */
  def descr(d: String): Unit = {
    editBuilder(_.copy(descr = d))
  }
}

/** Contains non-platform-specific functionality of ScallopConf. */
abstract class ScallopConfBase(
  val args: CSeq[String] = Nil,
  protected val commandNameAndAliases: Seq[String] = Nil
) extends ScallopConfValidations {

  /** Pointer to parent ScallopConf */
  protected var parentConfig: ScallopConfBase = this

  /** true if this config does not represent a subcommand */
  protected var isRootConfig = true

  private def rootConfig: ScallopConfBase = {
    var conf = this
    while (!conf.isRootConfig) {
      conf = conf.parentConfig
    }
    conf
  }

  /** List of sub-configs of this config. */
  protected var subconfigs: Seq[ScallopConfBase] = Nil

  /** Add subcommand to this config */
  def addSubcommand(conf: Subcommand): Unit = {
    subconfigs :+= conf

    conf.parentConfig = this
    conf.isRootConfig = false

    conf.verifyConf()
    conf.verified = true

    editBuilder(_.addSubBuilder(conf.commandNameAndAliases, conf.builder))
  }

  /** Internal immutable builder for options setup. */
  var builder = Scallop(args)

  private[scallop] def editBuilder(fn: Scallop => Scallop): Unit = {
    builder = fn(builder)
  }

  // machinery to support option name guessing
  protected var _guessOptionName: Boolean = true
  protected def optionNameGuessingSupported: Boolean
  protected def performOptionNameGuessing(): Unit

  /** If set to true, scallop would append auto-generated text about default option value
    * to option descriptions. */
  def appendDefaultToDescription = builder.appendDefaultToDescription
  /** If set to true, scallop would append auto-generated text about default option value
    * to option descriptions. */
  def appendDefaultToDescription_=(v: Boolean): Unit = {
    editBuilder(_.copy(appendDefaultToDescription = v))
  }

  /** Get current custom help formatter. */
  def helpFormatter = builder.helpFormatter
  /** Set custom help formatter. */
  def helpFormatter_=(formatter: ScallopHelpFormatter) = {
    editBuilder(_.copy(helpFormatter = formatter))
  }

  /** If set to true, then do not generate short names for subsequently defined options by default.
    * Only applied if a subsequent option definition does not explicitly provide its noshort-parameter.
    */
  def noshort = builder.noshort

  /** If set to true, then do not generate short names for subsequently defined options by default.
    * Only applied if a subsequent option definition does not explicitly provide its noshort-parameter.
    */
  def noshort_=(v: Boolean): Unit = {
    editBuilder(_.copy(noshort = v))
  }

  private[this] var gen = 0
  private[this] def genName() = { gen += 1; Util.format("\t%d", gen) }

  /** Retrieves the choosen subcommand. */
  def subcommand: Option[ScallopConfBase] = {
    assertVerified()
    builder.getSubcommandName.map(n => subconfigs.find(_.commandNameAndAliases.contains(n)).get)
  }

  /** Retrieves the list of the chosen nested subcommands. */
  def subcommands: List[ScallopConfBase] = {
    assertVerified()

    var config = this
    var configs = List[ScallopConfBase]()
    builder.getSubcommandNames.foreach { bn =>
      config = config.subconfigs.find(_.commandNameAndAliases.contains(bn)).get
      configs :+= config
    }
    configs
  }

  /** Get current prefix to command name (consists of parent builder names, separated by null char) */
  private def getPrefix = {
    var prefix = ""
    var conf = this
    while (!conf.isRootConfig) {
      prefix = conf.commandNameAndAliases.head + "\u0000" + prefix
      conf = conf.parentConfig
    }
    prefix
  }

  private def getPrefixedName(name: String) = getPrefix + name

  private[scallop] var verified = false

  /** Add a new simple option definition to this config.
    *
    * @param name Name for new option, used as long option name in parsing, and for option identification.
    * @param short By default, the first character of option name is used for short option name. You can override it by specifying the required character (`short = 'c'`).
    * @param descr Description for the option. Will be printed in help message, carefully formatted to the output width (80 characters by default).
    * @param default Default value to use if option is not found in input arguments (if you provide this, you can omit the type on method).
    * @param validate The function that validates the parsed value.
    * @param required Is this option required? Defaults to false.
    * @param argName The name for this option argument, as it will appear in help. Defaults to "arg".
    * @param hidden Hides description of this option from help (this can be useful for debugging options).
    * @param noshort If set to true, then this option does not have any short name.
    * @param group Option group to add this option to.
    * @param conv The converter for this option. Usually found implicitly.
    * @return ScallopOption, container for the parsed option value.
    */
  def opt[A](
    name: String = null,
    short: Char = '\u0000',
    descr: String = "",
    default: => Option[A] = None,
    validate: A => Boolean = (_:A) => true,
    required: Boolean = false,
    argName: String = "arg",
    hidden: Boolean = false,
    noshort: Boolean = builder.noshort,
    group: ScallopOptionGroup = null
  )(implicit conv:ValueConverter[A]): ScallopOption[A] = {

    // guessing name, if needed
    val resolvedName =
      if (name == null)
        if (_guessOptionName) {
          genName() // generate unique name, that will be replaced during verification with guessed name
        }
        else throw new IllegalArgumentException("You should supply a name for your option!")
      else name

    if (resolvedName.head.isDigit) {
      throw new IllegalOptionParameters(Util.format("First character of the option name must not be a digit: %s", resolvedName))
    }

    val defaultA =
      if (conv == flagConverter)
        { () =>
          if (default == Some(true)) Some(true)
          else Some(false)
        }
      else () => default

    val optionDescriptor = SimpleOption(
      name = resolvedName,
      short = if (short == '\u0000' || noshort) None else Some(short),
      descr = descr,
      required = required,
      converter = conv,
      default = defaultA,
      validator = { (a:Any) => validate(a.asInstanceOf[A]) },
      argName = argName,
      hidden = hidden,
      noshort = noshort
    )

    if (group != null) {
      group.options.append(optionDescriptor)
    }

    editBuilder(_.appendOption(optionDescriptor))

    new ScallopOption[A](() => resolvedName, Some(optionDescriptor)) {
      override lazy val fn = { (name: String) =>
        assertVerified()
        rootConfig.builder.get(getPrefixedName(name)).asInstanceOf[Option[A]]
      }
      override lazy val supplied = { (name: String) =>
        assertVerified()
        rootConfig.builder.isSupplied(getPrefixedName(name))
      }
    }
  }

  /** Add a new choice option definition to this config.
    *
    * This option takes a single string argument and restricts values to a list of possible choices.
    *
    * @param choices List of possible argument values.
    * @param name Name for new option, used as long option name in parsing, and for option identification.
    * @param short Overload the char that will be used as short option name. Defaults to first character of the name.
    * @param descr Description for this option, for help description.
    * @param default Default value to use if option is not found in input arguments (if you provide this, you can omit the type on method).
    * @param required Is this option required? Defaults to false.
    * @param argName The name for this option argument, as it will appear in help. Defaults to "arg".
    * @param hidden If set to true, then this option will be hidden from generated help output.
    * @param noshort If set to true, then this option does not have any short name.
    * @param group Option group to add this option to.
    * @param conv The converter for this option. Usually found implicitly.
    * @return ScallopOption, container for the parsed option value.
    */
  def choice(
    choices: Seq[String],
    name: String = null,
    short: Char = '\u0000',
    descr: String = "",
    default: => Option[String] = None,
    required: Boolean = false,
    argName: String = "arg",
    hidden: Boolean = false,
    noshort: Boolean = noshort,
    group: ScallopOptionGroup = null
  ): ScallopOption[String] = {
    this.opt[String](
      name = name,
      short = short,
      descr = this.helpFormatter.getChoiceHelpText(descr, choices),
      default = default,
      required = required,
      argName = argName,
      hidden = hidden,
      noshort = noshort,
      group = group
    )(new ValueConverter[String] {
      def parse(s: List[(String, List[String])]) = {
        s match {
          case (_, arg :: Nil) :: Nil =>
            if (choices.contains(arg)) {
              Right(Some(arg))
            } else {
              Left(s"Expected one of: ${choices.mkString(", ")}")
            }
          case Nil => Right(None)
          case _ => Left("You should provide exactly one argument for this option")
        }
      }
      val argType = ArgType.SINGLE
    })
  }

  /** Add a new tally option definition to this config.
    *
    * Tally options count how many times the option was provided on the command line.
    * E.g., `-vvv` will be countet as `3`.
    *
    * @param name Name for new option, used as long option name in parsing, and for option identification.
    * @param short Overload the char that will be used as short option name. Defaults to first character of the name.
    * @param descr Description for this option, for help description.
    * @param hidden If set to true, then this option will be hidden from generated help output.
    * @param noshort If set to true, then this option does not have any short name.
    * @param group Option group to add this option to.
    * @return ScallopOption, container for the parsed option value.
    */
  def tally(
    name: String = null,
    short: Char = '\u0000',
    descr: String = "",
    hidden: Boolean = false,
    noshort: Boolean = builder.noshort,
    group: ScallopOptionGroup = null
  ): ScallopOption[Int] = {

    // guessing name, if needed
    val resolvedName =
      if (name == null)
        if (_guessOptionName) genName()
        else throw new IllegalArgumentException("You should supply a name for your option!")
      else name

    val optionDescriptor = SimpleOption(
      name = resolvedName,
      short = if (short == '\u0000' || noshort) None else Some(short),
      descr = descr,
      required = false,
      converter = tallyConverter,
      default = () => Some(0),
      validator = _ => true,
      argName = "",
      hidden = hidden,
      noshort = noshort
    )

    if (group != null) {
      group.options.append(optionDescriptor)
    }

    editBuilder(_.appendOption(optionDescriptor))

    new ScallopOption[Int](() => resolvedName, Some(optionDescriptor)) {
      override lazy val fn = { (name: String) =>
        assertVerified()
        rootConfig.builder.get(getPrefixedName(name)).asInstanceOf[Option[Int]]
      }
      override lazy val supplied = { (name: String) =>
        assertVerified()
        rootConfig.builder.isSupplied(getPrefixedName(name))
      }
    }
  }

  /** Add new property option definition to this config object.
    *
    * This option will parse arguments like `-Dkey=value` or `-D key1=value1 key2=value2`.
    *
    * @param name Character that will be used as prefix for property arguments.
    * @param descr Description for this property option, for help description.
    * @param keyName Name for 'key' part of this option arg name, as it will appear in help option definition. Defaults to "key".
    * @param valueName Name for 'value' part of this option arg name, as it will appear in help option definition. Defaults to "value".
    * @param hidden If set to true, then this option will be hidden from generated help output.
    * @param group Option group to add this option to.
    * @param conv The converter for this option. Usually found implicitly.
    * @return ScallopOption, container for the parsed option value.
    */
  def props[A](
    name: Char = 'D',
    descr: String = "",
    keyName: String = "key",
    valueName: String = "value",
    hidden: Boolean = false,
    group: ScallopOptionGroup = null
  )(implicit conv: ValueConverter[Map[String,A]]): LazyMap[String, A] = {

    val optionDescriptor = PropertyOption(
      name = name.toString,
      short = name,
      descr = descr,
      converter = conv,
      keyName = keyName,
      valueName = valueName,
      hidden = hidden
    )

    if (group != null) {
      group.options.append(optionDescriptor)
    }

    editBuilder(_.appendOption(optionDescriptor))

    new LazyMap({
      assertVerified()
      rootConfig.builder.apply(getPrefixedName(name.toString)).asInstanceOf[Map[String, A]]
    }, Some(optionDescriptor))
  }


  /** Add new property option definition to this config object.
    *
    * This option will parse arguments like `--Props key1=value1 key2=value2`.
    *
    * @param name Name for new option, used as long option name in parsing, and for option identification.
    * @param descr Description for this property option, for help description.
    * @param keyName Name for 'key' part of this option arg name, as it will appear in help option definition. Defaults to "key".
    * @param valueName Name for 'value' part of this option arg name, as it will appear in help option definition. Defaults to "value".
    * @param hidden If set to true, then this option will be hidden from generated help output.
    * @param group Option group to add this option to.
    * @param conv The converter for this option. Usually found implicitly.
    * @return ScallopOption, container for the parsed option value.
    */
  def propsLong[A](
    name: String = "Props",
    descr: String = "",
    keyName: String = "key",
    valueName: String = "value",
    hidden: Boolean = false,
    group: ScallopOptionGroup = null
  )(implicit conv: ValueConverter[Map[String,A]]): Map[String, A] = {

    val optionDescriptor = LongNamedPropertyOption(
      name = name,
      descr = descr,
      converter = conv,
      keyName = keyName,
      valueName = valueName,
      hidden = hidden
    )

    if (group != null) {
      group.options.append(optionDescriptor)
    }

    editBuilder(_.appendOption(optionDescriptor))

    new LazyMap({
      assertVerified()
      rootConfig.builder.apply(getPrefixedName(name)).asInstanceOf[Map[String, A]]
    }, Some(optionDescriptor))
  }


  /** Add new trailing argument definition to this config.
    *
    * @param name Name for new definition, used for identification.
    * @param descr Description for this option, for help text.
    * @param validate The function that validates the parsed value.
    * @param required Is this trailing argument required? Defaults to true.
    * @param default If this argument is not required and not found in the argument list, use this value.
    * @param hidden If set to true then this option will not be present in auto-generated help.
    * @param group Option group to add this option to.
    * @param conv The converter for this option. Usually found implicitly.
    * @return ScallopOption, container for the parsed option value.
    */
  def trailArg[A](
    name: String = null,
    descr: String = "",
    validate: A => Boolean = (_:A) => true,
    required: Boolean = true,
    default: => Option[A] = None,
    hidden: Boolean = false,
    group: ScallopOptionGroup = null
  )(implicit conv:ValueConverter[A]): ScallopOption[A] = {

    val resolvedName = if (name == null) genName() else name

    val defaultA =
      if (conv == flagConverter)
        { () =>
          if (default == Some(true)) Some(true)
          else Some(false)
        }
      else () => default

    val optionDescriptor = TrailingArgsOption(
      name = resolvedName,
      required = required,
      descr = descr,
      converter = conv,
      validator = { (a:Any) => validate(a.asInstanceOf[A]) },
      default = defaultA,
      hidden = hidden
    )

    if (group != null) {
      group.options.append(optionDescriptor)
    }

    editBuilder(_.appendOption(optionDescriptor))

    new ScallopOption[A](() => resolvedName, Some(optionDescriptor)) {
      override lazy val fn = { (name: String) =>
        assertVerified()
        rootConfig.builder.get(getPrefixedName(name)).asInstanceOf[Option[A]]
      }
      override lazy val supplied = { (name: String) =>
        assertVerified()
        rootConfig.builder.isSupplied(getPrefixedName(name))
      }
    }
  }

  /** Add new number argument definition to this config and get a holder for it's value.
    *
    * Parses arguments like `-1` or `-3` (like GNU tail, for example).
    *
    * @param name Name for new definition, used for identification.
    * @param required Is this trailing argument required? Defaults to true.
    * @param descr Description for this option, for help text.
    * @param default If this argument is not required and not found in the argument list, use this value.
    * @param validate The function that validates the parsed value.
    * @param hidden If set to true then this option will not be present in auto-generated help.
    * @param group Option group to add this option to.
    * @param conv The converter for this option. Usually found implicitly.
    * @return ScallopOption, container for the parsed option value.
    */
  def number(
    name: String = null,
    descr: String = "",
    validate: Long => Boolean = (_:Long) => true,
    required: Boolean = false,
    default: => Option[Long] = None,
    hidden: Boolean = false,
    group: ScallopOptionGroup = null
  )(implicit conv: ValueConverter[Long]): ScallopOption[Long] = {

    val resolvedName =
      if (name == null) {
        if (_guessOptionName) genName()
        else throw new IllegalArgumentException("You should supply a name for your number option!")
      } else name

    val optionDescriptor = NumberArgOption(
      name = resolvedName,
      required = required,
      descr = descr,
      converter = conv,
      validator = { (a: Any) => validate(a.asInstanceOf[Long]) },
      default = () => default,
      hidden = hidden
    )

    if (group != null) {
      group.options.append(optionDescriptor)
    }

    editBuilder(_.appendOption(optionDescriptor))

    new ScallopOption[Long](() => resolvedName, Some(optionDescriptor)) {
      override lazy val fn = { (name: String) =>
        assertVerified()
        rootConfig.builder.get(getPrefixedName(name)).asInstanceOf[Option[Long]]
      }
      override lazy val supplied = { (name: String) =>
        assertVerified()
        rootConfig.builder.isSupplied(getPrefixedName(name))
      }
    }
  }

  /** Add new toggle option definition to this config, and get a holder for it's value.
    *
    * Toggle options are just glorified flag options. For example, if you create a
    * toggle option with name "verbose", it will be invocable in three ways -
    * "--verbose", "--noverbose", "-v".
    *
    * @param name Name of this option
    * @param default default value for this option
    * @param short Overload the char that will be used as short option name. Defaults to first character of the name.
    * @param noshort If set to true, then this option will not have any short name.
    * @param prefix Prefix to name of the option, that will be used for "negative" version of the option.
    * @param descrYes Description for positive variant of this option.
    * @param descrNo Description for negative variant of this option.
    * @param hidden If set to true, then this option will not be present in auto-generated help.
    * @param group Option group to add this option to.
    * @return ScallopOption, container for the parsed option value.
    */
  def toggle(
    name: String = null,
    default: => Option[Boolean] = None,
    short: Char = '\u0000',
    noshort: Boolean = noshort,
    prefix: String = "no",
    descrYes: String = "",
    descrNo: String = "",
    hidden: Boolean = false,
    group: ScallopOptionGroup = null
  ): ScallopOption[Boolean] = {

    val resolvedName =
      if (name == null) {
        if (_guessOptionName) genName()
        else throw new IllegalArgumentException("You should supply a name for your toggle!")
      } else name

    val optionDescriptor = ToggleOption(
      resolvedName,
      default = () => default,
      short = if (short == '\u0000' || noshort) None else Some(short),
      noshort = noshort,
      prefix = prefix,
      descrYes = descrYes,
      descrNo = descrNo,
      hidden = hidden
    )

    if (group != null) {
      group.options.append(optionDescriptor)
    }

    editBuilder(_.appendOption(optionDescriptor))

    new ScallopOption[Boolean](() => resolvedName, Some(optionDescriptor)) {
      override lazy val fn = { (name: String) =>
        assertVerified()
        rootConfig.builder.get(getPrefixedName(name)).asInstanceOf[Option[Boolean]]
      }
      override lazy val supplied = { (name: String) =>
        assertVerified()
        rootConfig.builder.isSupplied(getPrefixedName(name))
      }
    }
  }


  private var _mainOptions: () => Seq[CliOption] = () => Nil
  /** Options, that are to be printed first in the help printout */
  def mainOptions = _mainOptions()
  /** Set options, that are to be printed first in the help printout */
  @deprecated(
    "Use option groups instead, for example see https://github.com/scallop/scallop/wiki/Help-information-printing#option-groups",
    since = "Scallop 4.0.0"
  )
  def mainOptions_=(newMainOptions: => Seq[ScallopOption[_]]) = {
    _mainOptions = () => {
      newMainOptions.flatMap(_.cliOption)
    }
  }

  private val optionGroups: ArrayBuffer[ScallopOptionGroup] = new ArrayBuffer()
  /** Create and return a new option group */
  def group(header: String = ""): ScallopOptionGroup = {
    val newGroup = new ScallopOptionGroup(header)
    optionGroups.append(newGroup)
    newGroup
  }


  /** Verify that this config object is properly configured. */
  private[scallop] def verifyBuilder(): Unit = {
    try {
      verified = true
      builder.verify
      runValidations()
    } catch {
      case e: Exception =>
        onError(e)
    }
  }

  private[scallop] def runValidations(): Unit = {
    validations foreach { v =>
      v() match {
        case Right(_) =>
        case Left(err) => throw new ValidationFailure(err)
      }
    }

    for {
      subBuilder <- builder.getSubbuilder
      subConfig <- subconfigs.find(_.builder == subBuilder)
    } {
      subConfig.editBuilder(_.args(builder.getSubcommandArgs))
      subConfig.runValidations()
    }
  }

  /** This name would be included in output when reporting errors. */
  var printedName = "scallop"

  /** This function is called with the error message when ScallopException
    * occurs. By default, this function prints message (prefixed by *printedName*) to stdout,
    * coloring the output if possible, then calls `sys.exit(1)`.
    *
    * Update this variable with another function if you need to change that behavior.
    */
  var errorMessageHandler: String => Unit = (_) => {}

  /** This function is called in event of any exception
    * in arguments parsing. By default, it catches only
    * standard Scallop errors, letting all other pass through.
    */
  protected def onError(e: Throwable): Unit = e match {
    case r: ScallopResult if !throwError.value => r match {
      case Help("") =>
        builder.printHelp()
        Compat.exit(0)
      case Help(subname) =>
        builder.findSubbuilder(subname).get.printHelp()
        Compat.exit(0)
      case Version =>
        builder.vers.foreach(println)
        Compat.exit(0)
      case ScallopException(message) => errorMessageHandler(message)
      // following should never match, but just in case
      case other: exceptions.ScallopException => errorMessageHandler(other.getMessage)
    }
    case e => throw e
  }

  /** Checks that this Conf object is verified. If it is not, throws an exception. */
  def assertVerified(): Unit = {
    if (!verified) {
      throw new IncompleteBuildException()
    }
  }

  /** Adds a validation function to this configuration. This function will be run after all other verification steps.
    * @param fn Validation function. In case of error, it should return Left with the error message.
    */
  def addValidation(fn: => Either[String, Unit]): Unit = {
    validations :+= (() => fn)
  }

  /** Add a check that at least one of the options in the list was supplied if `opt` was supplied.
    * @param opt option, that depends on any of options in list
    * @param list list of dependencies (at least one will need to be present)
    */
  def dependsOnAny(opt: ScallopOption[_], list: List[ScallopOption[_]]) = addValidation {
    if (opt.isSupplied && !list.exists(_.isSupplied)) {
      Left(Util.format(
        "When specifying '%s', at least one of the following options must be provided: %s",
        opt.name, list.map(_.name).mkString(", ")
      ))
    } else Right(())
  }

  /** Add a check that all of the options in the list were also supplied if `opt` was supplied.
    * @param opt option that depends on all of options in list
    * @param list list of dependencies (all will need to be present)
    */
  def dependsOnAll(opt: ScallopOption[_], list: List[ScallopOption[_]]) = addValidation {
    if (opt.isSupplied && !list.forall(_.isSupplied)) {
      Left(Util.format(
        "When specifying '%s', all of the following options must also be provided: %s",
        opt.name, list.map(_.name).mkString(", ")
      ))
    } else Right(())
  }

  /** Add a check that none of the options in the list were supplied if `opt` was supplied.
    * @param opt option that conflicts with all of options in list
    * @param list list of dependencies (all will need to be absent)
    */
  def conflicts(opt: ScallopOption[_], list: List[ScallopOption[_]]) = addValidation {
    if (opt.isSupplied && list.exists(_.isSupplied)) {
      val conflict = list.find(_.isSupplied).get
      Left(Util.format("Option '%s' conflicts with option '%s'", opt.name, conflict.name))
    } else Right(())
  }

  /** Add a check that at least one of the options is supplied.
    * @param list list of options (at least one must be present)
    */
  def requireAtLeastOne(list: ScallopOption[_]*) = addValidation {
    if (!list.exists(_.isSupplied)) {
      Left(Util.format(
        "There should be at least one of the following options: %s",
        list.map(_.name).mkString(", ")
      ))
    } else Right(())
  }

  /** Add a check that at one and only one option in the list is supplied.
    * @param list list of conflicting options (exactly one must be present)
    */
  def requireOne(list: ScallopOption[_]*) = addValidation {
    if (list.count(_.isSupplied) != 1) {
      Left(Util.format(
        "There should be exactly one of the following options: %s",
        list.map(_.name).mkString(", ")
      ))
    } else Right(())
  }

  /** Add a check that only one or zero of the provided options have values supplied in arguments.
    * @param list list of mutually exclusive options
    */
  def mutuallyExclusive(list: ScallopOption[_]*) = addValidation {
    if (list.count(_.isSupplied) > 1) {
      Left(Util.format(
        "There should be only one or zero of the following options: %s",
        list.map(_.name).mkString(", ")
      ))
    } else Right(())
  }

  /** Add a check that either all or none of the provided options have values supplied in arguments.
    * @param list list of codependent options
    */
  def codependent(list: ScallopOption[_]*) = addValidation {
    val c = list.count(_.isSupplied)
    if (c != 0 && c != list.size) {
      Left(Util.format(
        "Either all or none of the following options should be supplied, because they are co-dependent: %s",
        list.map(_.name).mkString(", ")
      ))
    } else Right(())
  }

  /** Add a check that either all or none of the provided options
    * have values defined (either supplied in arguments or got from defaults).
    * @param list list of options
    */
  def allDefinedOrUndefined(list: ScallopOption[_]*) = addValidation {
    val c = list.count(_.toOption.isDefined)
    if (c != 0 && c != list.size) {
      Left(Util.format(
        "Either all or none of the following options should be defined, because they are co-dependent: %s",
        list.map(_.name).mkString(", ")
      ))
    } else Right(())
  }

  /** Validate that file exists. */
  def validateFileExists(fileOption: ScallopOption[File]) = addValidation {
    fileOption.toOption.fold[Either[String,Unit]](Right(())) { file =>
      if (!file.exists) {
        Left("File '" + file + "' not found")
      } else {
        Right(())
      }
    }
  }

  /** Validate that file does not exists. */
  def validateFileDoesNotExist(fileOption: ScallopOption[File]) = addValidation {
    fileOption.toOption
      .map(file => {
        if (file.exists()) Left(Util.format("File '%s' already exists", file))
        else Right(())
      })
      .getOrElse(Right(()))
  }

  /** Validate that file argument is directory. */
  def validateFileIsDirectory(fileOption: ScallopOption[File]) = addValidation {
    fileOption.toOption
      .map(file => {
        if (!file.isDirectory) Left(Util.format("File '%s' is not a directory", file))
        else Right(())
      })
      .getOrElse(Right(()))
  }

  /** Validate that file is not a directory. */
  def validateFileIsFile(fileOption: ScallopOption[File]) = addValidation {
    fileOption.toOption
      .map(file => {
        if (!file.isFile) Left(Util.format("File '%s' is not a file", file))
        else Right(())
      })
      .getOrElse(Right(()))
  }

  /** Validate that all the files in the arguments to multi-arg option exist. */
  def validateFilesExist(filesOption: ScallopOption[List[File]]) = addValidation {
    filesOption.toOption
      .map(files => {
        val problems = files.filterNot(_.exists)
        if (problems.nonEmpty) Left(Util.format("File(s) %s not found", Util.seqstr(problems)))
        else Right(())
      })
      .getOrElse(Right(()))
  }

  /** Validate that all the files in the arguments to multi-arg option do not exist. */
  def validateFilesDoNotExist(filesOption: ScallopOption[List[File]]) = addValidation {
    filesOption.toOption
      .map(files => {
        val problems = files.filter(_.exists)
        if (problems.nonEmpty) Left(Util.format("File(s) %s already exists", Util.seqstr(problems)))
        else Right(())
      })
      .getOrElse(Right(()))
  }

  /** Validate that all the files in the arguments to multi-arg option are directories. */
  def validateFilesIsDirectory(filesOption: ScallopOption[List[File]]) = addValidation {
    filesOption.toOption
      .map(files => {
        val problems = files.filterNot(_.isDirectory)
        if (problems.nonEmpty) Left(Util.format("File(s) %s is not a directory", Util.seqstr(problems)))
        else Right(())
      })
      .getOrElse(Right(()))
  }

  /** Validate that all the files in the arguments to multi-arg option are not directories. */
  def validateFilesIsFile(filesOption: ScallopOption[List[File]]) = addValidation {
    filesOption.toOption
      .map(files => {
        val problems = files.filterNot(_.isFile)
        if (problems.nonEmpty) Left(Util.format("File(s) %s is not a file", Util.seqstr(problems)))
        else Right(())
      })
      .getOrElse(Right(()))
  }

  /** Validate that path points to the existing file. */
  def validatePathExists(pathOption: ScallopOption[Path]) = addValidation {
    pathOption.toOption.fold[Either[String,Unit]](Right(())) { path =>
      if (!path.toFile.exists) {
        Left("File at '" + path + "' not found")
      } else {
        Right(())
      }
    }
  }

  /** Validate that path does not point to the existing file. */
  def validatePathDoesNotExist(pathOption: ScallopOption[Path]): Unit = addValidation {
    pathOption.toOption
      .map {
        case path if Files.exists(path) => Left(Util.format("File '%s' already exists", path))
        case _ => Right(())
      }
      .getOrElse(Right(()))
  }

  /** Validate that path points to a directory. */
  def validatePathIsDirectory(pathOption: ScallopOption[Path]): Unit = addValidation {
    pathOption.toOption
      .map {
        case path if Files.isDirectory(path) => Right(())
        case path => Left(Util.format("File '%s' is not a directory", path))
      }
      .getOrElse(Right(()))
  }

  /** Validate that path points to a file (not directory). */
  def validatePathIsFile(pathOption: ScallopOption[Path]): Unit = addValidation {
    pathOption.toOption
      .map {
        case path if Files.isRegularFile(path) => Right(())
        case path => Left(Util.format("File '%s' is not a directory", path))
      }
      .getOrElse(Right(()))
  }

  /** Validate that path target exists. */
  def validatePathsExists(pathsOption: ScallopOption[List[Path]]): Unit = addValidation {
    pathsOption.toOption
      .map(paths => {
        val problems = paths.filterNot(Files.exists(_))

        problems match {
          case Nil => Right(())
          case problem :: Nil => Left(Util.format("File %s not found", problem))
          case _ => Left(Util.format("Files %s not found", Util.seqstr(problems)))
        }
      })
      .getOrElse(Right(()))
  }

  /** Validate that path target does not exist. */
  def validatePathsDoesNotExist(pathsOption: ScallopOption[List[Path]]): Unit = addValidation {
    pathsOption.toOption
      .map(paths => {
        val problems = paths.filter(Files.exists(_))

        problems match {
          case Nil => Right(())
          case problem :: Nil => Left(Util.format("File %s already exists", problem))
          case _ => Left(Util.format("Files %s already exist", Util.seqstr(problems)))
        }
      })
      .getOrElse(Right(()))
  }

  /** Validate that paths targets exist. */
  def validatePathsIsDirectory(pathsOption: ScallopOption[List[Path]]): Unit = addValidation {
    pathsOption.toOption
      .map(paths => {
        val problems = paths.filterNot(Files.isDirectory(_))

        problems match {
          case Nil => Right(())
          case problem :: Nil => Left(Util.format("File %s is not a directory", problem))
          case _ => Left(Util.format("Files %s are not directories", Util.seqstr(problems)))
        }
      })
      .getOrElse(Right(()))
  }

  /** Validate that all paths targets are files (not directories). */
  def validatePathsIsFile(pathsOption: ScallopOption[List[Path]]): Unit = addValidation {
    pathsOption.toOption
      .map(paths => {
        val problems = paths.filterNot(Files.isRegularFile(_))

        problems match {
          case Nil => Right(())
          case problem :: Nil => Left(Util.format("File %s is not a file", problem))
          case _ => Left(Util.format("Files %s are not files", Util.seqstr(problems)))
        }
      })
      .getOrElse(Right(()))
  }

  /** Require subcommand to be provided (validation will fail if no subcommand was provided on the command line). */
  def requireSubcommand() = addValidation {
    if (subcommand.isEmpty) Left("Subcommand required")
    else Right(())
  }

  // === some getters for convenience ===

  /** Get summary of current parser state.
    *
    * @return a list of all options in the builder, and corresponding values for them.
    */
  def summary = {
    assertVerified()
    builder.summary
  }

  /** Get summary of current parser state, hididng values for some of the options.
    * Useful if you log the summary and want to avoid storing sensitive information
    * in the logs (like passwords)
    *
    * @param blurred names of the options that should be hidden.
    * @return a list of all options in the builder
    */
  def filteredSummary(blurred: Set[String]): String = {
    assertVerified()
    builder.filteredSummary(blurred: Set[String])
  }

  /** Get generated help contents as a string. */
  def getHelpString(): String = builder.help

  /** Prints help message (with version, banner, option usage and footer) to stdout. */
  def printHelp() = builder.printHelp()

  /** Add a version string to option builder.
    *
    * @param v Version string.
    */
  def version(v: String): Unit = {
    editBuilder(_.version(v))
  }

  /** Add a banner string to option builder.
    *
    * @param b Banner string.
    */
  def banner(b: String): Unit = {
    editBuilder(_.banner(b))
  }

  /** Add a footer string to this builder.
    *
    * @param f footer string.
    */
  def footer(f: String): Unit = {
    editBuilder(_.footer(f))
  }

  /** Explicitly set width of help printout. By default, Scallop tries
    * to determine it from terminal width or defaults to 80 characters.
    */
  def helpWidth(w: Int): Unit = {
    editBuilder(_.setHelpWidth(w))
  }

  /** If set to true, do not output subcommand options in the help output for the main program
    * (only output short subcommand description in such cases).
    * Full help for subcommand options can still be accessed via `program subcommand-name --help`.
    * @param enable enable short format for subcommand help
    */
  def shortSubcommandsHelp(enable: Boolean = true): Unit = {
    editBuilder(_.copy(shortSubcommandsHelp = enable))
  }

  private[scallop] def verifyConf(): Unit = {
    // pass option groups into the builder
    editBuilder(_.copy(
      mainOptions = _mainOptions().toList,
      optionGroups = optionGroups.toList.map { g =>
        (g.header, g.options.toSeq)
      }
    ))

    if (_guessOptionName) {
      performOptionNameGuessing()
    }

    if (builder.opts.exists(_.name.startsWith("\t"))) {
      if (optionNameGuessingSupported) {
        throw new OptionNameGuessingFailure()
      } else {
        throw new OptionNameGuessingUnsupported()
      }
    }
  }

  /** Verify this configuration - parse the arguments, convert option values, run validations.
    * This method MUST be called at the end of all options definitions, attempts to access
    * option values before it is called will result in runtime exception.
    */
  def verify(): Unit = {
    verifyConf()
    verifyBuilder()
  }

}

/* Convenience variables to permit testing. */
object throwError extends util.DynamicVariable[Boolean](false)
object overrideColorOutput extends util.DynamicVariable[Option[Boolean]](None)
