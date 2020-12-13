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
    Formatter.format(
      getOptionLines(s).flatMap {
        case Left(s) => List(Left(s))
        case Right(o) => o.helpInfo(s.getOptionShortNames(o)).map(Right(_))
      },
      s.helpWidth,
      needToAppendDefaultToDescription(s)
    )
  }

  protected def getOptionLines(s: Scallop): List[Either[String, CliOption]] = {
    joinWithEmptyLineSeparator(List(
      getMainOptionLines(s),
      getOptionGroupsLines(s),
      getNormalOptionLines(s) ++ getHelpLine(s) ++ getVersionLine(s)
    ))
  }

  protected def getMainOptionLines(s: Scallop): List[Either[String, CliOption]] = {
    s.mainOptions.map(Right(_))
  }

  protected def getOptionGroupsLines(s: Scallop): List[Either[String, CliOption]] = {
    joinWithEmptyLineSeparator(s.optionGroups.map { case (header, options) =>
      List(header).filter(_.nonEmpty).map(h => Left(" " + h)) ++ options.map(Right(_))
    })
  }

  protected def getNormalOptionLines(s: Scallop): List[Either[String, CliOption]] = {
    val optionsInGroups: Set[CliOption] = s.mainOptions.toSet ++ s.optionGroups.flatMap(_._2)

    s.opts
    .filter(!_.isPositional)
    .filter(!_.hidden)
    .filter(o => !optionsInGroups.contains(o))
    .filter(o => o.name != "help" && o.name != "version")
    .sortBy(_.name.toLowerCase)
    .map(o => Right(o))
  }

  protected def getHelpLine(s: Scallop): List[Either[String, CliOption]] = {
    val helpOption =
      s.opts.find(_.name == "help")
      .getOrElse(s.getHelpOption)
    if (helpOption.hidden) Nil
    else List(Right(helpOption))
  }

  protected def getVersionLine(s: Scallop): List[Either[String, CliOption]] = {
    val versionOption =
      s.opts.find(_.name == "version")
      .orElse(s.vers.flatMap(_ => s.getVersionOption))
      .filterNot(_.hidden)
    versionOption match {
      case None => Nil
      case Some(o) => List(Right(o))
    }
  }

  protected def getTrailingArgsHelp(s: Scallop): String = {
    val trailOpts = s.opts.filter(_.isPositional).filter(!_.hidden)
    if (trailOpts.isEmpty) {
      ""
    } else {
      val trailHelp = Formatter.format(
        trailOpts.flatMap(_.helpInfo(Nil)).map(Right(_)),
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
    }.mkString("\n\n")
  }

  protected def getSubcommandsWithNames(s: Scallop): List[(Scallop, List[String])] = {
    s.subbuilders
    .groupBy(_._2)
    .map { case (subBuilder, names) =>
      (subBuilder, names.map(_._1))
    }
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


  private def joinWithEmptyLineSeparator(optionLists: List[List[Either[String, CliOption]]]): List[Either[String, CliOption]] = {
    optionLists match {
      case first :: second :: rest =>
        if (first.nonEmpty && second.nonEmpty) {
          joinWithEmptyLineSeparator((first ::: List(Left("")) ::: second) :: rest)
        } else {
          joinWithEmptyLineSeparator((first ::: second) :: rest)
        }
      case _ =>
        optionLists.flatten
    }
  }

}
