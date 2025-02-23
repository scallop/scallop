package org.rogach.scallop.tokenize

sealed trait TokenizationResult

case class Matched(tokens: Seq[String], rest: StringView) extends TokenizationResult
case object Failed extends TokenizationResult
case class EOF(expected: String) extends TokenizationResult
