package org.rogach.scallop

import exceptions._
import scala.util.DynamicVariable
import reflect.runtime.universe._

class Subcommand(val commandName: String) extends ScallopConf(Nil, commandName) {
  /** Short description for this subcommand. Used if parent command has shortSubcommandsHelp enabled.
    */
  def descr(d: String) {
    editBuilder(_.copy(descr = d))
  }
}

abstract class ScallopConf(val args: Seq[String] = Nil, protected val commandname: String = "") extends ScallopConfValidations {

  /** Pointer to parent ScallopConf */
  protected var parentConfig: ScallopConf = this

  /** true if this config does not represent a subcommand */
  protected var isRootConfig = true

  private def rootConfig: ScallopConf = {
    var conf = this
    while (!conf.isRootConfig) {
      conf = conf.parentConfig
    }
    conf
  }

  /** List of sub-configs of this config. */
  protected var subconfigs: Seq[ScallopConf] = Nil

  /** Add subcommand to this config */
  def addSubcommand(conf: Subcommand) {
    subconfigs :+= conf

    conf.parentConfig = this
    conf.isRootConfig = false

    conf.verifyConf()

    editBuilder(_.addSubBuilder(conf.commandname, conf.builder))
  }

  var builder = Scallop(args)

  def editBuilder(fn: Scallop => Scallop) {
    builder = fn(builder)
  }

  // machinery to support option name guessing

  private[this] var _guessOptionName = true
  /** If true, scallop would try to guess missing option names from the names of their fields. */
  def guessOptionName = _guessOptionName
  /** If set to true, scallop would try to guess missing option names from the names of their fields. */
  def guessOptionName_=(v: Boolean) { _guessOptionName = v }

  private[this] var gen = 0
  private[this] def genName() = { gen += 1; "\t%d" format gen }

  /** Retrieves the choosen subcommand. */
  def subcommand: Option[ScallopConf] = {
    assertVerified
    assert(rootConfig == this, "You shouldn't call 'subcommand' on subcommand object")

    builder.getSubcommandName.map(n => subconfigs.find(_.commandname == n).get)
  }

  /** Retrieves the list of the chosen nested subcommands. */
  def subcommands: List[ScallopConf] = {
    assertVerified
    assert(rootConfig == this, "You shouldn't call 'subcommands' on subcommand object")

    var config = this
    var configs = List[ScallopConf]()
    builder.getSubcommandNames.foreach { bn =>
      config = config.subconfigs.find(_.commandname == bn).get
      configs :+= config
    }
    configs
  }

  /** Get current prefix to command name (consists of parent builder names, separated by null char) */
  private def getPrefix = {
    var prefix = ""
    var conf = this
    while (!conf.isRootConfig) {
      prefix = conf.commandname + "\u0000" + prefix
      conf = conf.parentConfig
    }
    prefix
  }

  private def getPrefixedName(name: String) = getPrefix + name

  var verified = false

  /** Add a new option definition to this config and get a holder for the value.
    *
    * @param name Name for new option, used as long option name in parsing, and for option identification.
    * @param short Overload the char that will be used as short option name. Defaults to first character of the name.
    * @param descr Description for this option, for help description.
    * @param default Default value to use if option is not found in input arguments (if you provide this, you can omit the type on method).
    * @param required Is this option required? Defaults to false.
    * @param argName The name for this option argument, as it will appear in help. Defaults to "arg".
    * @param noshort If set to true, then this option does not have any short name.
    * @param conv The converter for this option. Usually found implicitly.
    * @return A holder for parsed value
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
      noshort: Boolean = false)
      (implicit conv:ValueConverter[A]): ScallopOption[A] = {

    // guessing name, if needed
    val resolvedName =
      if (name == null)
        if (guessOptionName) {
          genName() // generate unique name, that will be replaced during verification with guessed name
        }
        else throw new IllegalArgumentException("You should supply a name for your option!")
      else name

    editBuilder(_.opt(resolvedName, short, descr, () => default, validate, required, argName, hidden, noshort)(conv))
    new ScallopOption[A](resolvedName) {
      override lazy val fn = { (name: String) =>
        assertVerified
        rootConfig.builder.get[A](getPrefixedName(name))(conv.tag)
      }
      override lazy val supplied = {
        assertVerified
        rootConfig.builder.isSupplied(getPrefixedName(name))
      }
    }
  }

  private var _mainOptions: () => Seq[String] = () => Nil
  /** Options, that are to be printed first in the help printout */
  def mainOptions = _mainOptions()
  /** Set options, that are to be printed first in the help printout */
  def mainOptions_=(newMainOptions: => Seq[ScallopOption[_]]) = {
    val prefix = getPrefix
    _mainOptions = () => {
      newMainOptions.map(_.name.stripPrefix(prefix))
    }
  }

  def tally(
      name: String = null,
      short: Char = '\u0000',
      descr: String = "",
      hidden: Boolean = false,
      noshort: Boolean = false): ScallopOption[Int] = {

    // guessing name, if needed
    val resolvedName =
      if (name == null)
        if (guessOptionName) genName()
        else throw new IllegalArgumentException("You should supply a name for your option!")
      else name

    editBuilder(
      _.opt[Int](resolvedName, short, descr, () => Some(0), _ => true,
                 false, "", hidden, noshort)(tallyConverter))
    new ScallopOption[Int](resolvedName) {
      override lazy val fn = { (name: String) =>
        assertVerified
        rootConfig.builder.get[Int](getPrefixedName(name))(implicitly[TypeTag[Int]])
      }
      override lazy val supplied = {
        assertVerified
        rootConfig.builder.isSupplied(getPrefixedName(name))
      }
    }
  }

  /** Add new property option definition to this config object, and get a handle for option retreiving.
    *
    * @param name Char, that will be used as prefix for property arguments.
    * @param descr Description for this property option, for help description.
    * @param keyName Name for 'key' part of this option arg name, as it will appear in help option definition. Defaults to "key".
    * @param valueName Name for 'value' part of this option arg name, as it will appear in help option definition. Defaults to "value".
    * @return A holder for retreival of the values.
    */
  def props[A](
      name: Char = 'D',
      descr: String = "",
      keyName: String = "key",
      valueName: String = "value",
      hidden: Boolean = false)
      (implicit conv: ValueConverter[Map[String,A]]): Map[String, A] = {
    editBuilder(_.props(name, descr, keyName, valueName, hidden)(conv))
    new LazyMap({
      assertVerified
      rootConfig.builder(getPrefixedName(name.toString))(conv.tag)
    })
  }

  def propsLong[A](
      name: String = "Props",
      descr: String = "",
      keyName: String = "key",
      valueName: String = "value",
      hidden: Boolean = false)
      (implicit conv: ValueConverter[Map[String,A]]): Map[String, A] = {
    editBuilder(_.propsLong(name, descr, keyName, valueName, hidden)(conv))
    new LazyMap({
      assertVerified
      rootConfig.builder(getPrefixedName(name))(conv.tag)
    })
  }


  /** Add new trailing argument definition to this config, and get a holder for it's value.
    *
    * @param name Name for new definition, used for identification.
    * @param required Is this trailing argument required? Defaults to true.
    * @param default If this argument is not required and not found in the argument list, use this value.
    */
  def trailArg[A](
      name: String = null,
      descr: String = "",
      validate: A => Boolean = (_:A) => true,
      required: Boolean = true,
      default: => Option[A] = None,
      hidden: Boolean = false)
      (implicit conv:ValueConverter[A]): ScallopOption[A] = {
    val resolvedName =
      if (name == null) {
        if (guessOptionName) genName()
        else throw new IllegalArgumentException("You should supply a name for your trailArg!")
      } else name
    editBuilder(_.trailArg(resolvedName, required, descr, () => default, validate, hidden)(conv))
    new ScallopOption[A](resolvedName) {
      override lazy val fn = { (name: String) =>
        assertVerified
        rootConfig.builder.get[A](getPrefixedName(name))(conv.tag)
      }
      override lazy val supplied = {
        assertVerified
        rootConfig.builder.isSupplied(getPrefixedName(name))
      }
    }
  }

  /** Add new toggle option definition to this config, and get a holder for it's value.
    *
    * Toggle options are just glorified flag options. For example, if you will ask for a
    * toggle option with name "verbose", it will be invocable in three ways -
    * "--verbose", "--noverbose", "-v".
    *
    * @param name Name of this option
    * @param default default value for this option
    * @param short Overload the char that will be used as short option name. Defaults to first character of the name.
    * @param noshort If set to true, then this option will not have any short name.
    * @param prefix Prefix to name of the option, that will be used for "negative" version of the
                    option.
    * @param descrYes Description for positive variant of this option.
    * @param descrNo Description for negative variant of this option.
    * @param hidden If set to true, then this option will not be present in auto-generated help.
    */
  def toggle(
      name: String = null,
      default: => Option[Boolean] = None,
      short: Char = '\u0000',
      noshort: Boolean = false,
      prefix: String = "no",
      descrYes: String = "",
      descrNo: String = "",
      hidden: Boolean = false): ScallopOption[Boolean] = {
    val resolvedName =
      if (name == null) {
        if (guessOptionName) genName()
        else throw new IllegalArgumentException("You should supply a name for your toggle!")
      } else name
    editBuilder(_.toggle(resolvedName, () => default, short, noshort, prefix, descrYes, descrNo, hidden))
    new ScallopOption[Boolean](resolvedName) {
      override lazy val fn = { (name: String) =>
        assertVerified
        rootConfig.builder.get[Boolean](getPrefixedName(name))
      }
      override lazy val supplied = {
        assertVerified
        rootConfig.builder.isSupplied(getPrefixedName(name))
      }
    }
  }

  /** Verify that this config object is properly configured. */
  private[scallop] def verifyBuilder() {
    try {
      verified = true
      builder.verify
      runValidations()
    } catch {
      case e: Throwable =>
        onError(e)
    }
  }

  private[scallop] def runValidations() {
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
      subConfig.runValidations
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
  var errorMessageHandler: String => Unit = { message =>
    if (System.console() == null) {
      // no colors on output
      println("[%s] Error: %s" format (printedName, message))
    } else {
      println("[\u001b[31m%s\u001b[0m] Error: %s" format (printedName, message))
    }
    sys.exit(1)
  }

  /** This function is called in event of any exception
    * in arguments parsing. By default, it catches only
    * standard Scallop errors, letting all other pass through.
    */
  protected def onError(e: Throwable): Unit = e match {
    case r: ScallopResult if !throwError.value => r match {
      case Help("") =>
        builder.printHelp
        sys.exit(0)
      case Help(subname) =>
        builder.findSubbuilder(subname).get.printHelp
        sys.exit(0)
      case Version =>
        builder.vers.foreach(println)
        sys.exit(0)
      case ScallopException(message) => errorMessageHandler(message)
    }
    case e => throw e
  }

  /** Checks that this Conf object is verified. If it is not, throws an exception. */
  def assertVerified() {
    if (!verified) {
      throw new IncompleteBuildException()
    }
  }

  private[scallop] def addValidation(fn: => Either[String, Unit]) {
    validations :+= (() => fn)
  }

  /** In the verify stage, if opt was supplied, checks that at least one of the options in list are also supplied.
    *
    * @param opt option, that depends on any of options in list
    * @param list list of dependencies (at least one will need to be present)
    */
  def dependsOnAny(opt: ScallopOption[_], list: List[ScallopOption[_]]) = addValidation {
    if (opt.isSupplied && !list.exists(_.isSupplied)) {
      Left("When specifying '%s', at least one of the following options must be provided: %s"
        format (opt.name, list.map(_.name).mkString(", ")))
    } else Right(Unit)
  }

  /** In the verify stage, if opt was supplied, checks that all of the options in list are also supplied.
    *
    * @param opt option, that depends on all of options in list
    * @param list list of dependencies (all will need to be present)
    */
  def dependsOnAll(opt: ScallopOption[_], list: List[ScallopOption[_]]) = addValidation {
    if (opt.isSupplied && !list.forall(_.isSupplied)) {
      Left("When specifying '%s', all of the following options must also be provided: %s"
        format (opt.name, list.map(_.name).mkString(", ")))
    } else Right(Unit)
  }

  /** In the verify stage, if opt was supplied, checks that all of the options in list are not supplied.
    *
    * @param opt option, that conflicts with all of options in list
    * @param list list of dependencies (all will need to be absent)
    */
  def conflicts(opt: ScallopOption[_], list: List[ScallopOption[_]]) = addValidation {
    if (opt.isSupplied && list.exists(_.isSupplied)) {
      val conflict = list.find(_.isSupplied).get
      Left("Option '%s' conflicts with option '%s'" format (opt.name, conflict.name))
    } else Right(Unit)
  }

  /** In the verify stage, checks that one, and only one, option in the list is supplied.
    *
    * @param list list of conflicting options (exactly one must be present)
    */
  def requireOne(list: ScallopOption[_]*) = addValidation {
    if (list.count(_.isSupplied) != 1) {
      Left("There should be exactly one of the following options: %s"
        format list.map(_.name).mkString(", "))
    } else Right(Unit)
  }

  /** In the verify stage, checks that only one or zero of the provided options have values supplied in arguments.
    *
    * @param list list of mutually exclusive options
    */
  def mutuallyExclusive(list: ScallopOption[_]*) = addValidation {
    if (list.count(_.isSupplied) > 1) {
      Left("There should be only one or zero of the following options: %s"
        format list.map(_.name).mkString(", "))
    } else Right(Unit)
  }

  /** In the verify stage, checks that either all or none of the provided options have values supplied in arguments.
    *
    * @param list list of codependent options
    */
  def codependent(list: ScallopOption[_]*) = addValidation {
    val c = list.count(_.isSupplied)
    if (c != 0 && c != list.size) {
      Left("Either all or none of the following options should be supplied, because they are co-dependent: %s"
        format list.map(_.name).mkString(", "))
    } else Right(Unit)
  }

  // === some getters for convenience ===

  /** Get summary of current parser state.
    *
    * @return a list of all options in the builder, and corresponding values for them.
    */
  def summary = {
    assertVerified
    builder.summary
  }

  /** Prints help message (with version, banner, option usage and footer) to stdout. */
  def printHelp() = builder.printHelp

  /** Add a version string to option builder.
    *
    * @param v Version string.
    */
  def version(v: String) {
    editBuilder(_.version(v))
  }

  /** Add a banner string to option builder.
    *
    * @param b Banner string.
    */
  def banner(b: String) {
    editBuilder(_.banner(b))
  }

  /** Add a footer string to this builder.
    *
    * @param f footer string.
    */
  def footer(f: String) {
    editBuilder(_.footer(f))
  }

  /** Explicitly set width of help printout. By default, Scallop tries
    * to determine it from terminal width or defaults to 80 characters.
    */
  def helpWidth(w: Int) {
    editBuilder(_.setHelpWidth(w))
  }

  def shortSubcommandsHelp(v: Boolean = true) {
    editBuilder(_.copy(shortSubcommandsHelp = v))
  }

  def verifyConf() {
    if (guessOptionName) {
      val methodsAndOptions =
        this.getClass.getMethods
        .filterNot(classOf[ScallopConf].getMethods.toSet)
        .filterNot(_.getName.endsWith("$eq"))
        .filterNot(_.getName.endsWith("$outer"))
        .filter(_.getReturnType == classOf[ScallopOption[_]])
        .filter(_.getParameterTypes.isEmpty)
        .map { m =>
          val opt = m.invoke(this).asInstanceOf[ScallopOption[_]]
          (m, opt)
        }
        .filter(_._2.name.contains("\t"))

      val nameMap = methodsAndOptions.sortBy(-_._2._transformCount).map { case (m, opt) =>
        val newName = m.getName.flatMap(c => if (c.isUpper) Seq('-', c.toLower) else Seq(c))
        // the old, generated version of name, without prefixes from parent builders
        val shortGenName = '\t' +: opt.name.reverse.takeWhile('\t'!=).reverse
        (opt.name, (shortGenName, newName))
      }.toMap

      methodsAndOptions.foreach { case (m, opt) =>
        val (shortGenName, newName) = nameMap(opt.name)
        editBuilder(e => e.copy(opts = e.opts.map { o =>
          if (o.name == shortGenName) {
            o match {
              case so: SimpleOption => so.copy(name = newName)
              case to: TrailingArgsOption => to.copy(name = newName)
              case to: ToggleOption => to.copy(name = newName)
              case _ => o
            }
          } else o
        }))
        opt._name = newName
      }
    }

    if (builder.opts.exists(_.name.startsWith("\t"))) {
      throw new OptionNameGuessingFailure()
    }

    // now, when we fixed option names, we can push mainOptions into the builder
    editBuilder(_.copy(mainOpts = _mainOptions().toList))

    verified = true
  }

  def verify() {
    verifyConf
    verifyBuilder
  }

}

/** Convenience variable to permit testing. */
object throwError extends util.DynamicVariable[Boolean](false)
