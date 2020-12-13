package org.rogach.scallop

import org.rogach.scallop.exceptions._

import scala.collection.{Seq => CSeq}

private[scallop] object Scallop {

  /** Create the new parser with some arguments already inserted.
    *
    * @param args Args to pre-insert.
    */
  def apply(args: CSeq[String]): Scallop = new Scallop(args)

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
      validator = (_) => true,
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
      validator = (_) => true,
      argName = "",
      hidden = false,
      noshort = true
    )
}

/** Internal configuration builder. */
case class Scallop(
  args: CSeq[String] = Nil,
  opts: List[CliOption] = Nil,
  mainOptions: List[CliOption] = Nil,
  optionGroups: List[(String, Seq[CliOption])] = Nil,
  vers: Option[String] = None,
  bann: Option[String] = None,
  foot: Option[String] = None,
  descr: String = "",
  helpWidth: Option[Int] = None,
  shortSubcommandsHelp: Boolean = false,
  appendDefaultToDescription: Boolean = false,
  noshort: Boolean = false,
  helpFormatter: ScallopHelpFormatter = new ScallopHelpFormatter,
  subbuilders: List[(String, Scallop)] = Nil
) extends ScallopArgListLoader {

  var parent: Option[Scallop] = None

  type Parsed = List[(CliOption, (String, List[String]))]

  case class ParseResult(
    opts: Parsed = Nil,
    subcommand: Option[String] = None,
    subcommandArgs: List[String] = Nil
  )

  /** Parse the argument into list of options and their arguments. */
  private def parse(args: CSeq[String]): ParseResult = {
    subbuilders.filter(s => args.contains(s._1)).sortBy(s => args.indexOf(s._1)).headOption match {
      case Some((name, sub)) =>
        ParseResult(
          parse(Nil, args.takeWhile(name != _).toList, Nil),
          Some(name),
          args.dropWhile(name != _).drop(1).toList
        )
      case None =>
        ParseResult(parse(Nil, args.toList, Nil))
    }
  }
  @annotation.tailrec
  private def parse(
    acc: Parsed,
    args: List[String],
    leadingArgsAcc: List[String]
  ): Parsed = {

    def goParseRest(
      leadingArgs: List[String],
      lastMultiArgOption: Option[(CliOption, String)],
      trailingArgs: List[String]
    ): Parsed = {
      def parseRest() = {
        val trailingOptions = opts.filter(_.isPositional)

        val result = TrailingArgumentsParser.parse(
          leadingArgs,
          lastMultiArgOption,
          trailingArgs,
          trailingOptions
        )
        result match {
          case TrailingArgumentsParser.ParseResult(_, _, excess) if excess.nonEmpty =>
            throw ExcessArguments(excess)

          case TrailingArgumentsParser.ParseResult(result, _, _) =>
            result.find(_._3.isLeft) match {
              case None =>
                result.flatMap {
                  case (option, invocation, Right(args)) =>
                    if (args.nonEmpty || option.required) {
                      List((option, (invocation, args)))
                    } else {
                      Nil
                    }
                  case _ => throw MajorInternalException()
                }
              case Some((option, _, Left((message, args)))) =>
                if (option.required && (message == "not enough arguments")) {
                  throw RequiredOptionNotFound(option.name)
                } else {
                  throw WrongOptionFormat(option.name, args.mkString(" "), message)
                }
              case _ => throw MajorInternalException()
            }
        }
      }

      lastMultiArgOption match {
        case Some((option, invocation)) =>
          option.converter.argType match {
            // handle simple option types immediately to avoid going into trailing args parsing with extra options
            case ArgType.FLAG =>
              (option, (invocation, Nil)) :: goParseRest(leadingArgs, None, trailingArgs)
            case ArgType.SINGLE =>
              if (trailingArgs.size > 0) {
                (option, (invocation, trailingArgs.take(1).toList)) :: goParseRest(leadingArgs, None, trailingArgs.tail)
              } else {
                throw new WrongOptionFormat(option.name, trailingArgs.mkString, "you should provide exactly one argument")
              }
            // short-circuit parsing when there are no trailing args - to get better error messages
            case ArgType.LIST if trailingArgs.isEmpty =>
              List(option -> ((invocation, Nil)))
            case ArgType.LIST => parseRest()
          }
        case None => parseRest()
      }
    }

    if (args.isEmpty) {
      if (leadingArgsAcc.isEmpty) {
        acc.reverse
      } else {
        // only trailing args left - proceed to trailing args parsing
        acc.reverse ::: goParseRest(Nil, None, leadingArgsAcc.reverse ::: removeFirstTrailingArgsSeparator(args))
      }
    } else if (args.head == "--") {
      // separator overrides any options that may follow, all remaining arguments go into trailing arguments
      acc.reverse ::: goParseRest(leadingArgsAcc.reverse, None, args.tail)
    } else if (isOptionName(args.head)) {
      if (args.head.startsWith("--")) {
        opts.find(_.longNames.exists(name => args.head.startsWith("--" + name + "="))) match {

          // parse --arg=value option style
          case Some(opt) =>
            val (invocation, arg) = args.head.drop(2).span('=' != _)
            parse(
              acc = (opt, (invocation, List(arg.drop(1)))) :: acc,
              args = args.tail,
              leadingArgsAcc = leadingArgsAcc
            )

          // parse --arg value... option style
          case None =>
            val optionName = args.head.drop(2)
            val option =
              opts.find(_.longNames.contains(optionName))
              .orElse(if (optionName == "help") Some(getHelpOption) else None)
              .orElse(if (optionName == "version") getVersionOption else None)
              .getOrElse(throw new UnknownOption(optionName))
            val (matchedArgs, remainingArgs) =
              option.converter.argType match {
                case ArgType.FLAG => (Nil, args.tail)
                case ArgType.SINGLE => (args.tail.take(1), args.tail.drop(1))
                case ArgType.LIST => args.tail.span(isArgument)
              }

            if (remainingArgs.isEmpty) {
              // proceed to trailing args parsing
              acc.reverse ::: goParseRest(leadingArgsAcc.reverse, Some((option, args.head.drop(2))), args.tail)
            } else {
              parse(
                acc = (option -> ((args.head.drop(2), matchedArgs.toList))) :: acc,
                args = remainingArgs,
                leadingArgsAcc = leadingArgsAcc
              )
            }
        }
      } else {
        if (args.head.size == 2) {
          val option = getOptionWithShortName(args.head(1)) getOrElse
                    (throw new UnknownOption(args.head.drop(1)))
          val (matchedArgs, remainingArgs) =
            option.converter.argType match {
              case ArgType.FLAG => (Nil, args.tail)
              case ArgType.SINGLE => (args.tail.take(1), args.tail.drop(1))
              case ArgType.LIST => args.tail.span(isArgument)
            }

          if (remainingArgs.isEmpty) {
            // proceed to trailing args parsing
            acc.reverse ::: goParseRest(leadingArgsAcc.reverse, Some((option, args.head.drop(1))), args.tail)
          } else {
            parse(
              acc = (option -> ((args.head.drop(1), matchedArgs.toList))) :: acc,
              args = remainingArgs,
              leadingArgsAcc = leadingArgsAcc
            )
          }
        } else {
          val option = getOptionWithShortName(args.head(1)) getOrElse
                    (throw new UnknownOption(args.head(1).toString))
          if (option.converter.argType != ArgType.FLAG) {
            parse(
              acc = acc,
              args = args.head.take(2) :: args.head.drop(2) :: args.tail,
              leadingArgsAcc = leadingArgsAcc
            )
          } else {
            parse(
              acc = acc,
              args = args.head.take(2) :: ("-" + args.head.drop(2)) :: args.tail,
              leadingArgsAcc = leadingArgsAcc
            )
          }
        }
      }
    } else if (args.head.matches("-[0-9]+")) {
      // parse number-only options
      val alreadyMatchedNumbers = acc.count(_._1.isInstanceOf[NumberArgOption])
      opts.filter(_.isInstanceOf[NumberArgOption]).drop(alreadyMatchedNumbers).headOption match {
        case Some(opt) =>
          val num = args.head.drop(1)
          parse(
            acc = (opt, (num, List(num))) :: acc,
            args = args.tail,
            leadingArgsAcc = leadingArgsAcc
          )
        case None =>
          parse(
            acc,
            args = args.tail,
            leadingArgsAcc = args.head :: leadingArgsAcc
          )
      }
    } else {
      // args.head is not an option, so it is a "leading trailing argument":
      // trailing argument that may be followed by some options
      parse(
        acc,
        args = args.tail,
        leadingArgsAcc = args.head :: leadingArgsAcc
      )
    }
  }

  /** Find an option, that responds to this short name. */
  def getOptionWithShortName(c: Char): Option[CliOption] = {
    opts
    .find(_.requiredShortNames.contains(c))
    .orElse {
      opts.find(_.shortNames.contains(c))
    }
    .orElse(Option(getHelpOption).find(_.requiredShortNames.contains(c)))
    .orElse(getVersionOption.find(_.requiredShortNames.contains(c)))
  }

  def getOptionShortNames(opt: CliOption): List[Char] = {
    (opt.shortNames ++ opt.requiredShortNames).distinct
    .filter(sh => getOptionWithShortName(sh).get == opt)
  }

  /** Result of parsing */
  private lazy val parsed: ParseResult = parse(loadArgList(args))

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

  private def removeFirstTrailingArgsSeparator(args: List[String]): List[String] = {
    val (argsBeforeSeparator, argsAfterSeparator) = args.span("--" != _)
    argsBeforeSeparator ::: argsAfterSeparator.drop(1)
  }

  def appendOption(option: CliOption): Scallop = {
    this.copy(opts = opts :+ option)
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
      val (firstSub, rest) = name.span('\u0000' != _)
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

  /** Get help on options from this builder. The resulting help is carefully formatted to required number of columns (default = 80, change with .setHelpWidth method),
    * and contains info on properties, options and trailing arguments.
    */
  def help: String = helpFormatter.formatHelp(this, "")

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
  def args(a: Seq[String]): Scallop = this.copy(args = args ++ a)

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
          .exists(_._1 == name.takeWhile('\u0000' != _))
        }
        .map { subBuilder =>
          subBuilder.args(parsed.subcommandArgs).isSupplied(name.dropWhile('\u0000' != _).drop(1))
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
     */
  def get(name: String): Option[Any] = {
    if (name.contains('\u0000')) {
      // delegating to subbuilder
      subbuilders.find(_._1 == name.takeWhile('\u0000' != _)).map(_._2.args(parsed.subcommandArgs).get(name.dropWhile('\u0000' != _).drop(1)))
        .getOrElse(throw new UnknownOption(name.replace("\u0000",".")))
    } else {
      opts.find(_.name == name).map { opt =>
        val args = parsed.opts.filter(_._1 == opt).map(_._2)
        opt.converter.parseCached(args) match {
          case Right(parseResult) =>
            parseResult.orElse(opt.default())
          case _ => if (opt.required) throw new MajorInternalException else None
        }
      }.getOrElse(throw new UnknownOption(name))
    }
  }

  def get(name: Char): Option[Any] = get(name.toString)

  /** Get the value of option. If option is not found, this will throw an exception.
    *
    * @param name Name for option.
    */
  def apply(name: String): Any = get(name).get

  def apply(name: Char): Any = apply(name.toString)

  def prop(name: Char, key: String): Option[Any] = apply(name).asInstanceOf[Map[String, Any]].get(key)

  lazy val getHelpOption =
    opts.find(_.name == "help")
    .getOrElse(
      if (opts.exists(opt => getOptionShortNames(opt).contains('h'))) {
        Scallop.builtinHelpOpt
      } else {
        Scallop.builtinHelpOpt.copy(short = Some('h'), noshort = false)
      }
    )

  lazy val getVersionOption =
    vers.map(_ => opts.find(_.name == "version")
    .getOrElse(
      if (opts.exists(opt => getOptionShortNames(opt).contains('v'))) {
        Scallop.builtinVersionOpt
      } else {
        Scallop.builtinVersionOpt.copy(short = Some('v'), noshort = false)
      }
    ))

  /** Verify the builder. Parses arguments, makes sure no definitions clash, no garbage or unknown options are present,
    * and all present arguments are in proper format. It is recommended to call this method before using the results.
    *
    * If there is "--help" or "--version" option present, it prints help or version statement and exits.
    */
  def verify: Scallop = {
    // option identifiers must not clash
    opts map (_.name) groupBy (a=>a) filter (_._2.size > 1) foreach
      (a => throw new IdenticalOptionNames(Util.format("Option identifier '%s' is not unique", a._1)))
    // long options names must not clash
    opts flatMap (_.longNames) groupBy (a=>a) filter (_._2.size > 1) foreach
      (a => throw new IdenticalOptionNames(Util.format("Long option name '%s' is not unique", a._1)))
    // short options names must not clash
    opts flatMap (o => (o.requiredShortNames).distinct) groupBy (a=>a) filter (_._2.size > 1) foreach
      (a => throw new IdenticalOptionNames(Util.format("Short option name '%s' is not unique", a._1)))


    val helpOpt = getHelpOption
    val shortHelpOpt = helpOpt match {
      case o: SimpleOption => o.short
      case _ => None
    }
    if (args.headOption == Some("--" + helpOpt.name) ||
        shortHelpOpt.map(s => args.headOption == Some("-" + s)).getOrElse(false)) {
      throw Help("")
    }

    getVersionOption.foreach { versionOpt =>
      val shortVersionOpt = versionOpt match {
        case o: SimpleOption => o.short
        case _ => None
      }
      if (args.headOption == Some("--" + versionOpt.name) ||
          shortVersionOpt.map(s => args.headOption == Some("-" + s)).getOrElse(false)) {
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
      if (!(get(o.name) map (v => o.validator(v)) getOrElse true))
        throw new ValidationFailure(Util.format("Validation failure for '%s' option parameters: %s", o.name, args.map(_._2.mkString(" ")).mkString(" ")))

    }

    this
  }

  /** Get summary of current parser state.
    *
    * Returns a list of all options in the builder, and corresponding values for them.
    */
  def summary: String = {
    Util.format("Scallop(%s)", args.mkString(", ")) + "\n" + filteredSummary(Set.empty)
  }

  /** Get summary of current parser state, hididng values for some of the options.
    * Useful if you log the summary and want to avoid storing sensitive information
    * in the logs (like passwords)
    *
    * @param blurred names of the options that should be hidden.
    * @return a list of all options in the builder
    */
  def filteredSummary(blurred: Set[String]): String = {
    lazy val hide = "************"
    opts.map { o =>
      Util.format(
        " %s  %s => %s",
        (if (isSupplied(o.name)) "*" else " "),
        o.name,
        if(!blurred.contains(o.name)) get(o.name).getOrElse("<None>") else hide
      )
    }.mkString("\n") + "\n" + parsed.subcommand.map { sn =>
      Util.format("subcommand: %s\n", sn) + subbuilders.find(_._1 == sn).get._2.args(parsed.subcommandArgs).filteredSummary(blurred)
    }.getOrElse("")
  }

}
