package org.rogach.scallop

import scala.collection.{Seq => CSeq}

/** Base class for CLI parsers. */
abstract class ScallopConf(
  args: CSeq[String] = Nil,
  commandNameAndAliases: Seq[String] = Nil
) extends ScallopConfBase(args, commandNameAndAliases) {

  override protected def optionNameGuessingSupported: Boolean = false
  override protected def performOptionNameGuessing(): Unit = {}

  errorMessageHandler = { message =>
    stderrPrintln(Util.format("[%s] Error: %s", printedName, message))
    exitHandler(1)
  }

}
