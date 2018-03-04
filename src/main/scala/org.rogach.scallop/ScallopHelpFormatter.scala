package org.rogach.scallop

class ScallopHelpFormatter {

  def formatHelp(s: Scallop, subcommandPrefix: String): String = {
    val formattedHelp =
      getOptionsHelp(s) +
      getTrailingArgsHelp(s) +
      getSubcommandsHelp(s, subcommandPrefix)

    // remove trailing whitespace
    formattedHelp.split("\n")
    .map(_.reverse.dropWhile(Character.isWhitespace).reverse)
    .mkString("\n")
  }

  protected def getOptionsHelp(s: Scallop): String = {
    Formatter format (
      getOptionLines(s) flatMap {
        case None => List(None)
        case Some(o) => o.helpInfo(s.getOptionShortNames(o)).map(Some(_))
      },
      s.helpWidth,
      needToAppendDefaultToDescription(s)
    )
  }

  protected def getOptionLines(s: Scallop): List[Option[CliOption]] = {
    getMainOptionLines(s) ++
    getNormalOptionLines(s) ++
    getHelpLine(s) ++
    getVersionLine(s)
  }

  protected def getMainOptionLines(s: Scallop): List[Option[CliOption]] = {
    s.mainOpts.map(mo => s.opts.find(_.name == mo)) ++
    // insert empty line before other options if main options are defined
    (if (s.mainOpts.nonEmpty) List(None) else Nil)
  }

  protected def getNormalOptionLines(s: Scallop): List[Option[CliOption]] = {
    s.opts
    .filter(!_.isPositional)
    .filter(!_.hidden)
    .filter(o => s.mainOpts.forall(o.name!=))
    .filter(o => o.name != "help" && o.name != "version")
    .sortBy(_.name.toLowerCase)
    .map(o => Some(o))
  }

  protected def getHelpLine(s: Scallop): List[Option[CliOption]] = {
    val helpOption =
      s.opts.find(_.name == "help")
      .getOrElse(s.getHelpOption)
    if (helpOption.hidden) Nil else List(Some(helpOption))
  }

  protected def getVersionLine(s: Scallop): List[Option[CliOption]] = {
    val versionOption =
      s.opts.find(_.name == "version")
      .orElse(s.vers.flatMap(_ => s.getVersionOption))
      .filterNot(_.hidden)
    if (versionOption.isDefined) List(versionOption) else Nil
  }

  protected def getTrailingArgsHelp(s: Scallop): String = {
    val trailOpts = s.opts.filter(_.isPositional).filter(!_.hidden)
    if (trailOpts.isEmpty) {
      ""
    } else {
      val trailHelp = Formatter format (
        trailOpts flatMap (_.helpInfo(Nil)) map (Some(_)),
        s.helpWidth,
        needToAppendDefaultToDescription(s)
      )
      "\n\n" + getTrailingArgsSectionName + "\n" + trailHelp
    }
  }

  protected def getTrailingArgsSectionName = " trailing arguments:"

  protected def getSubcommandsHelp(s: Scallop, subcommandPrefix: String): String = {
    if (s.subbuilders.isEmpty) {
      ""
    } else {
      if (s.shortSubcommandsHelp) {
        getShortSubcommandsHelp(s)
      } else {
        getLongSubcommandsHelp(s, subcommandPrefix)
      }
    }
  }

  protected def getShortSubcommandsHelp(s: Scallop): String = {
    val maxCommandLength = s.subbuilders.map(_._1.size).max

    "\n\n" + getSubcommandsSectionName + "\n" +
    s.subbuilders.map { case (name, option) =>
      "  " + name.padTo(maxCommandLength, ' ') + "   " + option.descr
    }.mkString("\n")
  }

  protected def getSubcommandsSectionName = "Subcommands:"

  protected def getLongSubcommandsHelp(s: Scallop, subcommandPrefix: String): String = {
    "\n\n" +
    getSubcommandsWithNames(s)
    .map { case (sub, names) =>
      getSubcommandHelp(sub, subcommandPrefix, names)
    }.mkString("\n")
  }

  protected def getSubcommandsWithNames(s: Scallop): List[(Scallop, List[String])] = {
    s.subbuilders
    .groupBy(_._2)
    .mapValues(_.map(_._1))
    .toList
    // retain original subcommand ordering
    .sortBy { case (subBuilder, names) => s.subbuilders.indexWhere(_._2 == subBuilder) }
  }

  protected def getSubcommandHelp(sub: Scallop, namePrefix: String, names: List[String]): String = {
    List(
      getSubcommandHeader(sub, namePrefix, names),
      sub.bann.getOrElse(""),
      getSubcommandOptionsHelp(sub, namePrefix + names.head + " "),
      sub.foot.getOrElse("")
    ).filter(_.nonEmpty).mkString("\n")
  }

  protected def getSubcommandHeader(sub: Scallop, namePrefix: String, names: List[String]): String = {
    val name =
      if (names.size == 1) names.head
      else names.head + " (alias: " + names.tail.mkString(", ") + ")"
    val description = if (sub.descr.nonEmpty) " - " + sub.descr else ""
    getSubcommandHeaderPrefix + namePrefix + name + description
  }

  protected def getSubcommandHeaderPrefix = "Subcommand: "

  protected def getSubcommandOptionsHelp(sub: Scallop, namePrefix: String): String = {
    this.formatHelp(sub, namePrefix).split("\n")
    .filter(!_.trim.startsWith("--version"))
    .mkString("\n")
  }

  protected def needToAppendDefaultToDescription(s: Scallop): Boolean =
    s.appendDefaultToDescription || s.parent.exists(needToAppendDefaultToDescription)

  def getChoiceHelpText(description: String, choices: Seq[String]): String =
    Seq(description, "Choices:", choices.mkString(", ")).filter(_.nonEmpty).mkString(" ")

}
