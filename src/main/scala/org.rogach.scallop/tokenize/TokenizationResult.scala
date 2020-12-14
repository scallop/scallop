package org.rogach.scallop.tokenize

private[scallop] sealed trait TokenizationResult

private[scallop] case class Matched(tokens: Seq[String], rest: StringView) extends TokenizationResult
private[scallop] case object Failed extends TokenizationResult
private[scallop] case class EOF(expected: String) extends TokenizationResult
