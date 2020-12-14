package org.rogach.scallop.tokenize

import java.lang.Character
import java.lang.StringBuilder

/** This class is responsible for correctly loading argument lists from stdin or files.
  * It handles splitting the input into individual arg tokens, accounting for quotes
  * and whitespace escaping.
  *
  * Tokenisation mechanism should be as similar to shell as possible - content in single quotes
  * is passed through as-is, content in double quotes allows escaping of `"` and `\` characters
  * via `\` character, and `\` outside quotes can be used to escape any character whatsoever.
  */
object ArgumentTokenizer {

  def tokenize(input: String): TokenizationResult = {
    argumentsParser(new StringView(input, 0))
  }

  private type Parser = StringView => TokenizationResult

  /// PARSER COMBINATORS

  /** Matches end of input. */
  private val inputEnd: Parser = (input: StringView) => {
    if (input.length == 0) Matched(Nil, input)
    else Failed
  }

  /** Converts Failed result from a parser into Matched(Nil),
    * Matched or EOF results are passed through unchanged.
    */
  private def optional(parser: Parser): Parser = (input: StringView) => {
    parser(input) match {
      case Failed => Matched(Nil, input)
      case other => other
    }
  }

  /** Creates a parser that applies parser1 and then parser2. */
  private def seq(parser1: Parser, parser2: Parser): Parser = (input: StringView) => {
    parser1(input) match {
      case eof: EOF => eof
      case Failed => Failed
      case Matched(tokens1, rest1) =>
        parser2(rest1) match {
          case Matched(tokens2, rest2) => Matched(tokens1 ++ tokens2, rest2)
          case other => other
        }
    }
  }

  /** Repeats the parser until getting Failed, concatenates all Matched results. */
  private def repeat(parser: Parser): Parser = (input: StringView) => {
    repeatImpl(parser, input, Nil)
  }
  @annotation.tailrec
  private def repeatImpl(parser: Parser, input: StringView, acc: List[Seq[String]]): TokenizationResult = {
    parser(input) match {
      case eof: EOF => eof
      case Failed =>
        if (acc.isEmpty) {
          Failed
        } else {
          Matched(acc.reverse.flatten, input)
        }
      case Matched(tokens, remainingInput) =>
        repeatImpl(parser, remainingInput, tokens :: acc)
    }
  }

  /** Iterates through provided parsers and returns the first Matched result. */
  private def alternatives(parsers: List[Parser]): Parser = (input: StringView) => {
    alternativesImpl(parsers, input)
  }
  @annotation.tailrec
  private def alternativesImpl(parsers: List[Parser], input: StringView): TokenizationResult = {
    parsers match {
      case Nil =>
        Failed
      case parser :: remainingParsers =>
        parser(input) match {
          case eof: EOF => eof
          case Failed =>
            alternativesImpl(remainingParsers, input)
          case matched: Matched =>
            matched
        }
    }
  }

  /** Transforms Matched result of a parser. */
  private def mapResult(parser: Parser, fn: Seq[String] => Seq[String]): Parser = (input: StringView) => {
    parser(input) match {
      case Matched(tokens, rest) => Matched(fn(tokens), rest)
      case other => other
    }
  }

  /// LOW-LEVEL PARSERS

  private val whitespace: Parser = (input: StringView) => {
    var i = 0
    val l = input.length
    while (i < l && Character.isWhitespace(input.charAt(i))) {
      i += 1
    }
    if (i == 0) {
      Failed
    } else {
      Matched(Nil, input.substring(i))
    }
  }

  private val singleQuotedString: Parser = (input: StringView) => {
    if (input.length == 0 || input.charAt(0) != '\'') {
      Failed
    } else {
      val stringEnd = input.indexOf('\'', 1)
      if (stringEnd == -1) {
        EOF("\'")
      } else {
        Matched(Seq(input.extract(1, stringEnd)), input.substring(stringEnd+1))
      }
    }
  }

  private val doubleQuotedString: Parser = (input: StringView) => {
    if (input.length == 0 || input.charAt(0) != '"') {
      Failed
    } else {
      var i = 1
      var s = 1
      val l = input.length
      val b = new StringBuilder()
      var break = false
      var eofEscaped = false
      var stringClosed = false
      while (!break && i < l) {
        while (i < l && input.charAt(i) != '\\' && input.charAt(i) != '"') {
          i += 1
        }
        b.append(input.extract(s, i))
        s = i
        if (i < l) {
          if (input.charAt(i) == '\\') {
            if (i + 1 < l) {
              if (input.charAt(i+1) == '\\' || input.charAt(i+1) == '"') {
                b.append(input.charAt(i+1))
              } else {
                b.append(input.extract(i, i+2))
              }
              i = i + 2
              s = i
            } else {
              eofEscaped = true
              break = true
            }
          } else if (input.charAt(i) == '"') {
            stringClosed = true
            break = true
          } else {
            b.append(input.charAt(i))
            i += 1
            s = i
          }
        }
      }
      if (eofEscaped) {
        EOF("escaped char")
      } else if (!stringClosed) {
        EOF("\"")
      } else {
        Matched(Seq(b.toString), input.substring(i+1))
      }
    }
  }

  private val plainArgument: Parser = (input: StringView) => {
    var i = 0
    var s = 0
    val l = input.length
    val b = new StringBuilder()
    var break = false
    var eofEscaped = false
    while (!break && i < l) {
      while (i < l && !Character.isWhitespace(input.charAt(i)) && input.charAt(i) != '\\' && input.charAt(i) != '"' && input.charAt(i) != '\'') {
        i += 1
      }
      b.append(input.extract(s, i))
      s = i
      if (i < l) {
        if (Character.isWhitespace(input.charAt(i))) {
          break = true
        } else if (input.charAt(i) == '\\') {
          if (i + 1 < l) {
            b.append(input.charAt(i+1))
            i = i + 2
            s = i
          } else {
            eofEscaped = true
            break = true
          }
        } else {
          break = true
        }
      }
    }
    if (eofEscaped) {
      EOF("escaped char")
    } else if (b.length() == 0) {
      Failed
    } else {
      Matched(Seq(b.toString), input.substring(i))
    }
  }

  private val token: Parser =
    mapResult(
      repeat(
        alternatives(List(
          singleQuotedString,
          doubleQuotedString,
          plainArgument
        ))
      ),
      tokens => Seq(tokens.mkString(""))
    )

  private val argumentsParser: Parser =
    alternatives(List(
      seq(
        seq(
          repeat(seq(optional(whitespace), token)),
          optional(whitespace)
        ),
        inputEnd
      ),
      seq(optional(whitespace), inputEnd)
    ))

}
