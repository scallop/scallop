package org.rogach.scallop

import org.rogach.scallop.exceptions.MajorInternalException

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
    result: List[(CliOption, String, Either[(String, List[String]), List[String]])],
    failedArgumentsCount: Int,
    excessArguments: List[String]
  ) extends Ordered[ParseResult] {

    lazy val failedConverters = result.count(_._3.isLeft)

    def compare(other: ParseResult): Int = {
      if (this.failedConverters < other.failedConverters) -1
      else if (this.failedConverters > other.failedConverters) 1
      else this.failedArgumentsCount.compare(other.failedArgumentsCount)
    }

    def isFailed = failedArgumentsCount > 0 || excessArguments.nonEmpty || result.exists(_._3.isLeft)
  }

  /** Run parse.
    *
    * @param leadingArgs trailing arguments before last multi-arg option (will be Nil if there was no last multi-arg option)
    * @param lastMultiArgOption option definition and invocation string forthe last multi-arg option (if it was present)
    * @param trailingArgs trailing arguments after the last multi-arg option (including arguments to that option if it is present)
    * @param trailingOptions list of trailing option definitions
    */
  def parse(
    leadingArgs: List[String],
    lastMultiArgOption: Option[(CliOption, String)],
    trailingArgs: List[String],
    trailingOptions: List[CliOption],
    resultAcc: ParseResult = ParseResult(Nil, 0, Nil)
  ): ParseResult = {

    def inner(
      args: List[String],
      option: CliOption,
      invocation: String,
      resultAcc: ParseResult,
      recur: (ParseResult, List[String], List[String]) => ParseResult // (parseResultSoFar, matchedArgs, remainingArgs)
    ): ParseResult = {
      // store best match so far, return it in case of error
      var bestMatch: Option[ParseResult] = None

      val minLength = if (option.converter.argType != ArgType.FLAG && option.required) 1 else 0
      val maxLength = option.converter.argType match {
        case ArgType.FLAG => 0
        case ArgType.SINGLE => math.min(1, args.size)
        case ArgType.LIST => args.size
      }

      if (minLength > maxLength) {
        return recur(
          ParseResult(
            result = (option, invocation, Left(("not enough arguments", args))) :: resultAcc.result,
            failedArgumentsCount = resultAcc.failedArgumentsCount + args.size,
            excessArguments = Nil
          ),
          Nil,
          Nil
        )
      }

      // start with maximum allowed length for this argument type,
      // try matching, if failed, reduce length by 1 and repeat
      var matchLength = maxLength

      while (matchLength >= minLength) {
        val (matchedArgs, restArgs) = args.splitAt(matchLength)

        val thisResult =
          if (option.required || matchLength > 0)
            option.converter.parseCached(List((invocation, matchedArgs)))
          else Right(Nil)

        val result: ParseResult = thisResult match {
          case Right(_) =>
            recur(
              resultAcc.copy(result = (option, invocation, Right(matchedArgs)) :: resultAcc.result),
              matchedArgs,
              restArgs
            )
          case Left(error) =>
            recur(
              resultAcc.copy(
                result = (option, invocation, Left((error, matchedArgs))) :: resultAcc.result,
                failedArgumentsCount = resultAcc.failedArgumentsCount + matchedArgs.size
              ),
              matchedArgs,
              restArgs
            )
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

    if (leadingArgs.nonEmpty) {
      trailingOptions match {
        case Nil =>
          // some leading arguments are still left, but there are no trailing arguments to match them
          resultAcc.copy(
            result = resultAcc.result.reverse,
            failedArgumentsCount = resultAcc.failedArgumentsCount + leadingArgs.size + trailingArgs.size,
            excessArguments = leadingArgs
          )
        case trailingOption :: restOfTrailingOptions =>
          lastMultiArgOption match {
            case None =>
              parse(Nil, None, leadingArgs ::: trailingArgs, trailingOptions, resultAcc)
            case Some((multiArgOption, multiArgOptionInvocation)) =>
              inner(
                args = leadingArgs,
                option = trailingOption,
                invocation = "",
                resultAcc = resultAcc,
                recur = (trailingOptionResultAcc, trailingOptionMatchedArgs, trailingOptionRestArgs) => {
                  if (trailingOptionRestArgs.nonEmpty) {
                    parse(trailingOptionRestArgs, lastMultiArgOption, trailingArgs, restOfTrailingOptions, trailingOptionResultAcc)
                  } else {
                    // The worst case.
                    // We now have to parse the last multi-arg option. But it can be preceded and followed by arguments
                    // for the same trailing arg option - for example in "t1 t2 --opt a1 a2 t3 t4",
                    // we need to correctly attribute t1,t2,t3,t4 to single trailing option.
                    // So here we need to parse multi-arg option, then take arguments for the previously parsed
                    // trailing option, prepend them to the rest of the arguments and re-parse the trailing option.
                    inner(
                      args = trailingArgs,
                      option = multiArgOption,
                      invocation = multiArgOptionInvocation,
                      resultAcc = trailingOptionResultAcc,
                      recur = (multiArgOptionResultAcc, multiArgOptionMatchedArgs, multiArgOptionRestArgs) => {
                        multiArgOptionResultAcc.result match {
                          case multiArgOptionResult :: prevTrailOptionResult :: restResult =>
                            parse(
                              leadingArgs = Nil,
                              lastMultiArgOption = None,
                              trailingArgs = trailingOptionMatchedArgs ::: multiArgOptionRestArgs,
                              trailingOptions = trailingOption :: restOfTrailingOptions,
                              resultAcc = multiArgOptionResultAcc.copy(
                                result = multiArgOptionResult :: restResult
                              )
                            )
                          case _ =>
                            throw MajorInternalException()
                        }
                      }
                    )
                  }
                }
              )
          }
      }
    } else {
      (lastMultiArgOption, trailingOptions) match {
        case (None, Nil) =>
          if (trailingArgs.isEmpty) {
            resultAcc.copy(
              result = resultAcc.result.reverse
            )
          } else {
            // some arguments are still left, and there are no converters to match them
            resultAcc.copy(
              result = resultAcc.result.reverse,
              failedArgumentsCount = resultAcc.failedArgumentsCount + trailingArgs.size,
              excessArguments = trailingArgs
            )
          }
        case (Some((multiArgOption, invocation)), _) =>
          inner(
            args = trailingArgs,
            option = multiArgOption,
            invocation = invocation,
            resultAcc = resultAcc,
            recur = (result, matchedArgs, restArgs) => {
              // This multi-arg option was not preceded by any trailing arguments,
              // so here we can just recurse into further parsing
              parse(Nil, None, restArgs, trailingOptions, result)
            }
          )
        case (None, trailingOption :: restOfTrailingOptions) =>
          inner(
            args = trailingArgs,
            option = trailingOption,
            invocation = "",
            resultAcc = resultAcc,
            recur = (result, matchedArgs, restArgs) => {
              parse(Nil, None, restArgs, restOfTrailingOptions, result)
            }
          )
      }
    }
  }
}
