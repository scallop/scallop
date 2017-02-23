package org.rogach.scallop

import org.rogach.scallop.exceptions._
import reflect.runtime.universe._

/** The creator and god of all parsers :) */
object Scallop {

  /** Create the new parser with some arguments already inserted.
    *
    * @param args Args to pre-insert.
    */
  def apply(args: Seq[String]): Scallop = new Scallop(args)

  /** Create the default empty parser, fresh as mountain air. */
  def apply(): Scallop = apply(Nil)

  private[scallop] def builtinHelpOpt =
    SimpleOption(
      name = "help",
      short = None,
      descr = "Show help message",
      required = false,
      converter = flagConverter,
      default = () => None,
      validator = (_,_) => true,
      argName = "",
      hidden = false,
      noshort = true
    )

  private[scallop] def builtinVersionOpt =
    SimpleOption(
      name = "version",
      short = None,
      descr = "Show version of this program",
      required = false,
      converter = flagConverter,
      default = () => None,
      validator = (_,_) => true,
      argName = "",
      hidden = false,
      noshort = true

    )
}

/** The main builder class.
  *
  * @param args Arguments to parse.
  * @param opts Options definitions.
  * @param mainOpts Names of options, that are to be printed first in the help printout
  * @param vers Version string to display in help.
  * @param bann Banner (summary of this program and command-line usage) to display in help.
  * @param foot Footer - displayed after options.
  * @param descr Short description - used for subcommands
  * @param helpWidth Width, to which the help output will be formatted (note that banner, footer, version and description are not affected!)
  * @param shortSubcommandsHelp If true, then help output from this builder wouldn't list full help for subcommands, only short description
  * @param subbuilders subcommands in this builder
  */
case class Scallop(
    args: Seq[String] = Nil,
    opts: List[CliOption] = Nil,
    mainOpts: List[String] = Nil,
    vers: Option[String] = None,
    bann: Option[String] = None,
    foot: Option[String] = None,
    descr: String = "",
    helpWidth: Option[Int] = None,
    shortSubcommandsHelp: Boolean = false,
    appendDefaultToDescription: Boolean = false,
    subbuilders: List[(String, Scallop)] = Nil,
    var parent: Option[Scallop] = None
  ) {

  type Parsed = List[(CliOption, (String, List[String]))]

  case class ParseResult(
    opts: Parsed = Nil,
    subcommand: Option[String] = None,
    subcommandArgs: List[String] = Nil
  )

  /** Parse the argument into list of options and their arguments. */
  private def parse(args: Seq[String]): ParseResult = {
    subbuilders.filter(s => args.contains(s._1)).sortBy(s => args.indexOf(s._1)).headOption match {
      case Some((name, sub)) => ParseResult(parse(Nil, args.takeWhile(name!=).toList), Some(name), args.dropWhile(name!=).drop(1).toList)
      case None => ParseResult(parse(Nil, args.toList))
    }
  }
  @annotation.tailrec
  private def parse(acc: Parsed, args: List[String]): Parsed = {
    def goParseRest(args: List[String], opt: Option[(String, CliOption)]): Parsed = {
      def parseRest = {
        val trailingOptions =
          opt.map(o => (o._2, o._1, true)).toList :::
          opts.filter(_.isPositional).map(o => (o, "", o.required))
        val trailingConverters = trailingOptions.map {
          case (opt, invocation, required) => (opt.converter, invocation, required)
        }

        val res = TrailingArgumentsParser.parse(args.toList, trailingConverters)
        res match {
          case TrailingArgumentsParser.ParseResult(_, _, excess) if excess.nonEmpty =>
            throw ExcessArguments(excess)

          case TrailingArgumentsParser.ParseResult(result, _, _) if result.exists(_.isLeft) =>
            val ((option, _, _), Left((message, args))) =
              trailingOptions.zip(result).find(_._2.isLeft).get
            throw WrongOptionFormat(option.name, args.mkString(" "), message)

          case TrailingArgumentsParser.ParseResult(result, _, _) =>
            trailingOptions.zip(result).flatMap {
              case ((option, invocation, required), Right(args)) =>
                if (args.nonEmpty || required) {
                  List((option, (invocation, args)))
                } else Nil
              case _ => throw MajorInternalException()
            }
        }
      }

      opt match {
        case Some((invoc, o)) =>
          // short-circuit parsing when there are no trailing args - to get better error messages
          o.converter.argType match {
            case ArgType.FLAG   =>
              (o, (invoc, Nil)) :: goParseRest(args, None)
            case ArgType.SINGLE =>
              if (args.size > 0) {
                (o, (invoc, args.take(1).toList)) :: goParseRest(args.tail, None)
              } else {
                throw new WrongOptionFormat(o.name, args.mkString, "you should provide exactly one argument")
              }
            case ArgType.LIST if args.isEmpty => List(o -> ((invoc, Nil)))
            case ArgType.LIST => parseRest
          }
        case None => parseRest
      }
    }

    if (args.isEmpty) acc.reverse
    else if (isOptionName(args.head) && args.head != "--") {
      if (args.head.startsWith("--")) {
        opts.find(_.longNames.exists(name => args.head.startsWith("--" + name + "="))) match {

          // pase --arg=value option style
          case Some(opt) =>
            val (invocation, arg) = args.head.drop(2).span('=' != _)
            parse(acc = (opt, (invocation, List(arg.drop(1)))) :: acc, args = args.tail)

          // parse usual --arg value... option style
          case None =>
            val opt = opts find (_.longNames.contains(args.head.drop(2))) getOrElse
                      (throw new UnknownOption(args.head.drop(2)))
            val (before, after) = args.tail.span(isArgument)
            if (after.isEmpty) {
              // get the converter, proceed to trailing args parsing
              acc.reverse ::: goParseRest(args.tail, Some((args.head.drop(2),opt)))
            } else {
              parse( acc = (opt -> ((args.head.drop(2), before.toList))) :: acc,
                     args = after)
            }
        }
      } else {
        if (args.head.size == 2) {
          val opt = getOptionWithShortName(args.head(1)) getOrElse
                    (throw new UnknownOption(args.head.drop(1)))
          val (before, after) = args.tail.span(isArgument)
          if (after.isEmpty) {
            // get the converter, proceed to trailing args parsing
            acc.reverse ::: goParseRest(args.tail, Some((args.head.drop(1), opt)))
          } else {
            parse( acc = (opt -> ((args.head.drop(1), before.toList))) :: acc,
                   args = after)
          }
        } else {
          val opt = getOptionWithShortName(args.head(1)) getOrElse
                    (throw new UnknownOption(args.head(1).toString))
          if (opt.converter.argType != ArgType.FLAG) {
            parse(acc, args.head.take(2) :: args.head.drop(2) :: args.tail)
          } else {
            parse(acc, args.head.take(2) :: ("-" + args.head.drop(2)) :: args.tail)
          }
        }
      }
    } else if (args.head.matches("-[0-9]+")) {
      // parse number-only options
      val alreadyMatchedNumbers = acc.count(_._1.isInstanceOf[NumberArgOption])
      opts.filter(_.isInstanceOf[NumberArgOption]).drop(alreadyMatchedNumbers).headOption match {
        case Some(opt) =>
          val num = args.head.drop(1)
          parse(acc = (opt, (num, List(num))) :: acc, args = args.tail)
        case None =>
          throw new UnknownOption(args.head)
      }
    } else {
      // only trailing args left - proceed to trailing args parsing
      val trailArgs = if (args.head == "--") args.tail else args
      acc.reverse ::: goParseRest(trailArgs, None)
    }
  }

  /** Find an option, that responds to this short name. */
  def getOptionWithShortName(c: Char): Option[CliOption] = {
    opts find (_.requiredShortNames.contains(c)) orElse {
      opts find (_.shortNames.contains(c))
    }
  }

  def getOptionShortNames(opt: CliOption): List[Char] = {
    (opt.shortNames ++ opt.requiredShortNames).distinct.filter(sh => getOptionWithShortName(sh).get == opt)
  }

  /** Result of parsing */
  private lazy val parsed: ParseResult = if (args.headOption map("@--" ==) getOrElse false) {
    // read options from stdin
    val argList =
      io.Source.fromInputStream(java.lang.System.in).getLines.toList
      .flatMap(_.split(" ").filter(_.size > 0))
    parse(argList)
  } else if (args.headOption map(_ startsWith "@") getOrElse false) {
    // read options from a file (canned config)
    val argList =
      io.Source.fromFile(args.head.drop(1)).getLines.toList
      .flatMap(_.split(" ").filter(_.size > 0))
    parse(argList)
  } else parse(args)

  /** Tests whether this string contains option name, not some number. */
  private def isOptionName(s: String) =
    if (s.startsWith("-"))
      if (s.size > 1)
        !s(1).isDigit
      else if (s.size == 1)
        false
      else true
    else false

  /** Tests whether this string contains option parameter, not option call. */
  private def isArgument(s: String) = !isOptionName(s)


  /** Add a new option definition to this builder.
    *
    * @param name Name for new option, used as long option name in parsing, and for option identification.
    * @param short Overload the char that will be used as short option name.
                   Defaults to first character of the name.
    * @param descr Description for this option, for help description.
    * @param default Default value to use if option is not found in input arguments
                     (if you provide this, you can omit the type on method).
    * @param required Is this option required? Defaults to false.
    * @param argName The name for this ortion argument, as it will appear in help. Defaults to "arg".
    * @param noshort If set to true, then this option does not have any short name.
    * @param conv The converter for this option. Usually found implicitly.
    * @param validate The function, that validates the parsed value
    * @param hidden Hides description of this option from help (this can be useful for debugging options)
    */
  def opt[A](
      name: String,
      short: Char = '\u0000',
      descr: String = "",
      default: () => Option[A] = () => None,
      validate: A => Boolean = ((_:A) => true),
      required: Boolean = false,
      argName: String = "arg",
      hidden: Boolean = false,
      noshort: Boolean = false)
      (implicit conv: ValueConverter[A]): Scallop = {
    if (name.head.isDigit) throw new IllegalOptionParameters("First character of the option name must not be a digit: %s" format name)
    val defaultA =
      if (conv == flagConverter)
        { () =>
          if (default() == Some(true)) Some(true)
          else Some(false)
        }
      else default
    val eShort = if (short == '\u0000' || noshort) None else Some(short)
    val validator = { (tt:TypeTag[_], a:Any) =>
      if (conv.tag.tpe <:< tt.tpe) validate(a.asInstanceOf[A])
      else false
    }
    this.copy(opts = opts :+ SimpleOption(name,
                                          eShort,
                                          descr,
                                          required,
                                          conv,
                                          defaultA,
                                          validator,
                                          argName,
                                          hidden,
                                          noshort))
  }

  /** Add new property option definition to this builder.
    *
    * @param name Char, that will be used as prefix for property arguments.
    * @param descr Description for this property option, for help description.
    * @param keyName Name for 'key' part of this option arg name, as it will appear in help option definition. Defaults to "key".
    * @param valueName Name for 'value' part of this option arg name, as it will appear in help option definition. Defaults to "value".
    */
  def props[A](
      name: Char,
      descr: String = "",
      keyName: String = "key",
      valueName: String = "value",
      hidden: Boolean = false)
      (implicit conv: ValueConverter[Map[String,A]]): Scallop =
    this.copy(opts = opts :+ PropertyOption(name.toString, name, descr, conv, keyName, valueName, hidden))

  def propsLong[A](
      name: String,
      descr: String = "",
      keyName: String = "key",
      valueName: String = "value",
      hidden: Boolean = false)
      (implicit conv: ValueConverter[Map[String,A]]): Scallop =
    this.copy(opts = opts :+ LongNamedPropertyOption(name,
                                                     descr,
                                                     conv,
                                                     keyName,
                                                     valueName,
                                                     hidden))

  /** Add new trailing argument definition to this builder.
    *
    * @param name Name for new definition, used for identification.
    * @param required Is this trailing argument required? Defaults to true.
    * @param descr Description for this option, for help text.
    * @param default If this argument is not required and not found in the argument list, use this value.
    * @param validate The function, that validates the parsed value
    */
  def trailArg[A](
      name: String,
      required: Boolean = true,
      descr: String = "",
      default: () => Option[A] = () => None,
      validate: A => Boolean = ((_:A) => true),
      hidden: Boolean = false)
      (implicit conv: ValueConverter[A]): Scallop = {
    val defaultA =
      if (conv == flagConverter)
        { () =>
          if (default() == Some(true)) Some(true)
          else Some(false)
        }
      else default
    val validator = { (tt: TypeTag[_], a:Any) =>
      if (conv.tag.tpe <:< tt.tpe) validate(a.asInstanceOf[A])
      else false
    }
    this.copy(opts = opts :+ TrailingArgsOption(name,
                                                required,
                                                descr,
                                                conv,
                                                validator,
                                                defaultA,
                                                hidden))
  }

  /** Add new number argument definition to this builder.
    *
    * @param name Name for new definition, used for identification.
    * @param required Is this trailing argument required? Defaults to true.
    * @param descr Description for this option, for help text.
    * @param default If this argument is not required and not found in the argument list, use this value.
    * @param validate The function that validates the parsed value.
    * @param hidden If set to true then this option will not be present in auto-generated help.
    */
  def number(
      name: String,
      required: Boolean = false,
      descr: String = "",
      default: () => Option[Long] = () => None,
      validate: Long => Boolean = ((_:Long) => true),
      hidden: Boolean = false)
      (implicit conv: ValueConverter[Long]): Scallop = {

    val validator = { (tt: TypeTag[_], a: Any) =>
      if (conv.tag.tpe <:< tt.tpe) validate(a.asInstanceOf[Long])
      else false
    }
    this.copy(opts = opts :+ NumberArgOption(
      name,
      required,
      descr,
      conv,
      validator,
      default,
      hidden
    ))
  }

  /** Add new toggle option definition to this builer.
    *
    * @param name  Name for new definition, used for identification.
    * @param default Default value
    * @param short Name for short form of this option
    * @param noshort If set to true, then this option will not have any short name.
    * @param prefix Prefix to name of the option, that will be used for "negative" version of the
                    option.
    * @param descrYes Description for positive variant of this option.
    * @param descrNo Description for negative variant of this option.
    * @param hidden If set to true, then this option will not be present in auto-generated help.
    */
  def toggle(
      name: String,
      default: () => Option[Boolean] = () => None,
      short: Char = '\u0000',
      noshort: Boolean = false,
      prefix: String = "no",
      descrYes: String = "",
      descrNo: String = "",
      hidden: Boolean = false) = {
    val eShort = if (short == '\u0000' || noshort) None else Some(short)
    this.copy(opts = opts :+ ToggleOption(name,
                                          default,
                                          eShort,
                                          noshort,
                                          prefix,
                                          descrYes,
                                          descrNo,
                                          hidden))
  }

  /** Adds a subbuilder (subcommand) to this builder.
    * @param name All arguments after this string would be routed to this builder.
    */
  def addSubBuilder(nameAndAliases: Seq[String], builder: Scallop) = {
    builder.parent = Some(this)
    this.copy(subbuilders = subbuilders ++ nameAndAliases.map(name => name -> builder))
  }

  /** Traverses the tree of subbuilders, using the provided name.
    * @param name Names of subcommand names, that lead to the needed builder, separated by \\0.
    */
  def findSubbuilder(name: String): Option[Scallop] = {
    if (name.contains('\u0000')) {
      val (firstSub, rest) = name.span('\u0000'!=)
      subbuilders.find(_._1 == firstSub).flatMap(_._2.findSubbuilder(rest.tail))
    } else subbuilders.find(_._1 == name).map(_._2)
  }

  /** Retrieves name of the subcommand that was found in input arguments. */
  def getSubcommandName = parsed.subcommand

  /** Retrieves the subbuilder object,
    * that matches the name of the subcommand found in input arguments. */
  def getSubbuilder: Option[Scallop] = parsed.subcommand.flatMap { sn =>
    subbuilders.find(_._1 == sn).map(_._2)
  }

  /** Returns the subcommand arguments. */
  def getSubcommandArgs: List[String] = parsed.subcommandArgs

  /** Returns the list of subcommand names, recursively. */
  def getSubcommandNames: List[String] = {
    parsed.subcommand.map(subName => subbuilders.find(_._1 == subName).map(s => s._1 :: s._2.args(parsed.subcommandArgs).getSubcommandNames).getOrElse(Nil)).getOrElse(Nil)
  }

  /** Retrieves a list of all supplied options (including options from subbuilders). */
  def getAllSuppliedOptionNames: List[String] = {
    opts.map(_.name).filter(isSupplied) ::: parsed.subcommand.map(subName => subbuilders.find(_._1 == subName).map(s => s._2.args(parsed.subcommandArgs)).get.getAllSuppliedOptionNames.map(subName + "\u0000" + _)).getOrElse(Nil)
  }

  /** Add version string to this builder.
    *
    * @param v Version string, to be printed before all other things in help.
    */
  def version(v: String) = this.copy(vers = Some(v))

  /** Add banner string to this builder. Banner should describe your program and provide a short
    * summary on it's usage.
    *
    * @param b Banner string, can contain multiple lines. Note this is not formatted to 80 characters!
    */
  def banner(b: String) = this.copy(bann = Some(b))

  /** Add footer string to this builder. Footer will be printed in help after option definitions.
    *
    * @param f Footer string, can contain multiple lines. Note this is not formatted to 80 characters!
    */
  def footer(f: String) = this.copy(foot = Some(f))

  /** Explicitly sets the needed width for the help printout. */
  def setHelpWidth(w: Int) = this.copy(helpWidth = Some(w))

  private def needToAppendDefaultToDescription: Boolean =
    appendDefaultToDescription || parent.exists(_.needToAppendDefaultToDescription)

  /** Get help on options from this builder. The resulting help is carefully formatted to required number of columns (default = 80, change with .setHelpWidth method),
    * and contains info on properties, options and trailing arguments.
    */
  def help: String = help("")

  private def help(subcommandPrefix: String): String = {
    // --help and --version do not go through normal pipeline, so we need to hardcode them here
    val helpOpt = opts.find(_.name == "help").getOrElse(Scallop.builtinHelpOpt)
    val versionOpt = opts.find(_.name == "version").orElse(vers.map(_ => Scallop.builtinVersionOpt))

    val optsToFormat =
      mainOpts.map(mo => opts.find(_.name == mo)) ++
      // insert empty line before other options if main options are defined
      mainOpts.headOption.map(_=>List(None)).getOrElse(Nil) ++
      (opts
        filter (!_.isPositional)
        filter (!_.hidden)
        filter (o => mainOpts.forall(o.name!=))
        filter (o => o.name != "help" && o.name != "version")
        sortBy (_.name.toLowerCase)
        map (o => Some(o))
      ) ++
      List(Some(helpOpt), versionOpt).map(_.filterNot(_.hidden)).filter(_.isDefined)

    val optsHelp = Formatter format (
      optsToFormat flatMap {
        case None => List(None)
        case Some(o) => o.helpInfo(getOptionShortNames(o)).map(Some(_))
      },
      helpWidth,
      needToAppendDefaultToDescription
    )

    val subcommandsHelp = if (shortSubcommandsHelp) {
      subbuilders.headOption.map { _ =>
        val maxCommandLength = subbuilders.map(_._1.size).max
        "\n\nSubcommands:\n" + subbuilders.map(s => "  " + s._1.padTo(maxCommandLength, ' ') + "   " + s._2.descr).mkString("\n")
      }.getOrElse("")
    } else {
      val subHelp = subbuilders
        .groupBy(_._2)
        .mapValues(_.map(_._1))
        .toList
        .sortBy { case (subBuilder, names) => subbuilders.indexWhere(_._2 == subBuilder) }
        .map { case (sub, names) =>
          val subName =
            if (names.size == 1) names.head
            else names.head + " (alias: " + names.tail.mkString(", ") + ")"
          val subDescr = if (sub.descr.nonEmpty) " - " + sub.descr else ""
          ("Subcommand: %s%s%s" format (subcommandPrefix, subName, subDescr)) + "\n" +
          sub.bann.map(_+"\n").getOrElse("") +
          sub.help(subcommandPrefix + names.head + " ").split("\n").
            filter(!_.trim.startsWith("--version")).mkString("\n") +
          sub.foot.map("\n"+_).getOrElse("")
        }.mkString("\n")
      if (subHelp.nonEmpty) "\n\n" + subHelp else subHelp
    }
    val trailOpts = opts filter (_.isPositional) filter (!_.hidden)
    val trailHelp = Formatter format (
      trailOpts flatMap (_.helpInfo(Nil)) map (Some(_)),
      helpWidth,
      needToAppendDefaultToDescription
    )
    val formattedHelp = if (trailOpts isEmpty) {
      optsHelp + subcommandsHelp
    } else {
      optsHelp + "\n\n trailing arguments:\n" + trailHelp + subcommandsHelp
    }
    formattedHelp.replaceAll(" +(\n)| +$", "$1") // remove trailing whitespace
  }

  /** Print help message (with version, banner, option usage and footer) to stdout. */
  def printHelp() = {
    vers foreach println
    bann foreach println
    println(help)
    foot foreach println
  }

  /** Add some more arguments to this builder. They are appended to the end of the original list.
    *
    * @param a arg list to add
    */
  def args(a: Seq[String]) = this.copy(args = args ++ a)

  /** Tests if this option or trailing arg was explicitly provided by argument list (not from default).
    *
    * @param name Identifier of option or trailing arg definition
    */
  def isSupplied(name: String): Boolean = {
    if (name.contains('\u0000')) {
      // delegating to subbuilder
      parsed.subcommand.map { subc =>
        subbuilders
        .find(_._1 == subc).map(_._2)
        .filter { subBuilder =>
          subbuilders.filter(_._2 == subBuilder)
          .exists(_._1 == name.takeWhile('\u0000'!=))
        }
        .map { subBuilder =>
          subBuilder.args(parsed.subcommandArgs).isSupplied(name.dropWhile('\u0000'!=).drop(1))
        }.getOrElse(false) // only current subcommand can have supplied arguments
      }.getOrElse(false) // no subcommands, so their options are definitely not supplied
    } else {
      opts find (_.name == name) map { opt =>
        val args = parsed.opts.filter(_._1 == opt).map(_._2)
        opt.converter.parseCached(args) match {
          case Right(Some(_)) => true
          case _ => false
        }
      } getOrElse(throw new UnknownOption(name))
    }
  }

   /** Get the value of option (or trailing arg) as Option.
     * @param name Name for option.
     * @param tt TypeTag for requested type. Usually found implicitly.
     */
  def get[A](name: String)(implicit tt: TypeTag[A]): Option[A] = {
    if (name.contains('\u0000')) {
      // delegating to subbuilder
      subbuilders.find(_._1 == name.takeWhile('\u0000'!=)).map(_._2.args(parsed.subcommandArgs).get(name.dropWhile('\u0000'!=).drop(1))(tt))
        .getOrElse(throw new UnknownOption(name.replace("\u0000","."))).asInstanceOf[Option[A]]
    } else {
      opts.find(_.name == name).map { opt =>
        if (!(opt.converter.tag.tpe <:< tt.tpe))
          throw new WrongTypeRequest(tt, opt.converter.tag)
        val args = parsed.opts.filter(_._1 == opt).map(_._2)
        opt.converter.parseCached(args) match {
          case Right(parseResult) =>
            parseResult.orElse(opt.default())
          case _ => if (opt.required) throw new MajorInternalException else None
        }
      }.getOrElse(throw new UnknownOption(name)).asInstanceOf[Option[A]]
    }
  }

  def get[A](name: Char)(implicit tt: TypeTag[A]): Option[A] = get(name.toString)(tt)

  /** Get the value of option. If option is not found, this will throw an exception.
    *
    * @param name Name for option.
    * @param tt TypeTag for requested type. Usually found implicitly.
    */
  def apply[A](name: String)(implicit tt: TypeTag[A]): A = get(name)(tt).get

  def apply[A](name: Char)(implicit tt: TypeTag[A]): A = apply(name.toString)(tt)

  def prop[A](name: Char, key: String)(implicit tt: TypeTag[Map[String, A]]): Option[A] = apply(name)(tt).get(key)

  /** Verify the builder. Parses arguments, makes sure no definitions clash, no garbage or unknown options are present,
    * and all present arguments are in proper format. It is recommended to call this method before using the results.
    *
    * If there is "--help" or "--version" option present, it prints help or version statement and exits.
    */
  def verify: Scallop = {
    // option identifiers must not clash
    opts map (_.name) groupBy (a=>a) filter (_._2.size > 1) foreach
      (a => throw new IdenticalOptionNames("Option identifier '%s' is not unique" format a._1))
    // long options names must not clash
    opts flatMap (_.longNames) groupBy (a=>a) filter (_._2.size > 1) foreach
      (a => throw new IdenticalOptionNames("Long option name '%s' is not unique" format a._1))
    // short options names must not clash
    opts flatMap (o => (o.requiredShortNames).distinct) groupBy (a=>a) filter (_._2.size > 1) foreach
      (a => throw new IdenticalOptionNames("Short option name '%s' is not unique" format a._1))


    val helpOpt = opts.find(_.name == "help").getOrElse(Scallop.builtinHelpOpt)
    val shortHelpOpt = helpOpt match {
      case o: SimpleOption => o.short
      case _ => None
    }
    if (args.headOption == Some("--" + helpOpt.name) ||
        shortHelpOpt.map(s => args.headOption == Some("-" + s)).getOrElse(false)) {
      throw Help("")
    }

    vers.foreach { _ =>
      val versionOpt = opts.find(_.name == "version").getOrElse(Scallop.builtinVersionOpt)
      val shortVersionOpt = versionOpt match {
        case o: SimpleOption => o.short
        case _ => None
      }
      if (args.contains("--" + versionOpt.name) ||
          shortVersionOpt.map(s => args.contains("-" + s)).getOrElse(false)) {
        throw Version
      }
    }

    parsed

    // verify subcommand parsing
    parsed.subcommand.map { sn =>
      subbuilders.find(_._1 == sn).map { case (sn, sub)=>
        try {
          sub.args(parsed.subcommandArgs).verify
        } catch {
          case Help("") => throw Help(sn)
          case h @ Help(subname) => throw Help(sn + "\u0000" + subname)
        }
      }
    }

    opts foreach { o =>
      val args = parsed.opts filter (_._1 == o) map (_._2)
      val res = o.converter.parseCached(args)
      res match {
        case Left(msg) =>
          throw new WrongOptionFormat(o.name, args.map(_._2.mkString(" ")).mkString(" "), msg)
        case _ =>
      }
      if (o.required && !res.fold(_ => false, _.isDefined) && !o.default().isDefined)
        throw new RequiredOptionNotFound(o.name)
      // validaiton
      if (!(get(o.name)(o.converter.tag) map (v => o.validator(o.converter.tag,v)) getOrElse true))
        throw new ValidationFailure("Validation failure for '%s' option parameters: %s" format (o.name, args.map(_._2.mkString(" ")).mkString(" ")))

    }

    this
  }

  /** Get summary of current parser state.
    *
    * Returns a list of all options in the builder, and corresponding values for them.
    */
  def summary: String = {
    ("Scallop(%s)" format args.mkString(", ")) + "\n" + filteredSummary(Set.empty)
  }

  /** Get summary of current parser state + blurring the values of parameters provided.
    *
    * @param blurred names of arguments that should be hidden.
    *
    * Returns a list of all options in the builder, and corresponding values for them
    * with eventually blurred values.
    */
  def filteredSummary(blurred:Set[String]): String = {
    lazy val hide = "************"
    opts.map { o =>
      " %s  %s => %s" format((if (isSupplied(o.name)) "*" else " "),
        o.name,
        if(!blurred.contains(o.name)) get(o.name)(o.converter.tag).getOrElse("<None>") else hide)
    }.mkString("\n") + "\n" + parsed.subcommand.map { sn =>
      ("subcommand: %s\n" format sn) + subbuilders.find(_._1 == sn).get._2.args(parsed.subcommandArgs).filteredSummary(blurred)
    }.getOrElse("")
  }

}
