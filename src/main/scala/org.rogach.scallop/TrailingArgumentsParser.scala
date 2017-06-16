package org.rogach.scallop

/** Parses the trailing arguments (including the arguments to last option).
  * Uses simple backtraking algorithm.
  */
object TrailingArgumentsParser {

  /** Contains error messages or matched argument lists for each option,
    * also storing number of arguments that failed to match and excess arguments.
    *
    * Also defines ordering for selecting best-match failure from list of failures.
    */
  case class ParseResult(
    result: List[Either[(String, List[String]), List[String]]],
    failedArguments: Int,
    excessArguments: List[String]
  ) extends Ordered[ParseResult] {

    lazy val failedConverters = result.count(_.isLeft)

    def compare(other: ParseResult): Int = {
      if (this.failedConverters < other.failedConverters) -1
      else if (this.failedConverters > other.failedConverters) 1
      else this.failedArguments.compare(other.failedArguments)
    }

    def isFailed = failedArguments > 0 || excessArguments.nonEmpty || result.exists(_.isLeft)
  }

  /** Run parse.
    *
    * @param args arguments to parse
    * @param convs list of (converter, invocation, required flag)
    */
  def parse(args: List[String], convs: List[(ValueConverter[_], String, Boolean)]): ParseResult = {
    convs match {
      case Nil =>
        if (args.isEmpty) {
          ParseResult(Nil, 0, Nil)
        } else {
          // some arguments are still left, and there are no converters to match them
          ParseResult(Nil, args.size, args)
        }

      case (converter, invocation, mustMatch) :: rest =>
        // store best match so far, return it in case of error
        var bestMatch: Option[ParseResult] = None

        // start with maximum allowed length for this argument type,
        // try matching, if failed, reduce length by 1 and repeat
        var matchLength = converter.argType match {
          case ArgType.FLAG => 0
          case ArgType.SINGLE => math.min(1, args.size)
          case ArgType.LIST => args.size
          case argType => sys.error("unsupported arg type: " + argType)
        }
        val minLength = if (converter.argType != ArgType.FLAG && mustMatch) 1 else 0

        if (minLength > matchLength) {
          val ParseResult(restResult, failCount, excess) = parse(Nil, rest)
          return ParseResult(Left(("not enough arguments", args)) :: restResult, failCount + args.size, excess)
        }

        while (matchLength >= minLength) {
          val (matchArgs, restArgs) = args.splitAt(matchLength)

          val thisResult =
            if (mustMatch || matchLength > 0)
              converter.parseCached(List((invocation, matchArgs)))
            else Right(Nil)

          val ParseResult(restResult, failCount, excess) = parse(restArgs, rest)

          val result = thisResult match {
            case Right(_) =>
              ParseResult(Right(matchArgs) :: restResult, failCount, excess)
            case Left(error) =>
              ParseResult(Left((error, matchArgs)) :: restResult, failCount + matchArgs.size, excess)
          }
          if (!result.isFailed) {
            return result
          } else {
            bestMatch = bestMatch match {
              case Some(oldBest) if oldBest.compare(result) <= 0 => Some(oldBest)
              case _ => Some(result)
            }
          }

          matchLength -= 1
        }

        bestMatch.get
    }
  }
}
