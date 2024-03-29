package org.rogach.scallop

/** Base class for CLI parsers. */
abstract class ScallopConf(
  args: Seq[String] = Nil,
  commandNameAndAliases: Seq[String] = Nil
) extends ScallopConfBase(args, commandNameAndAliases) {

  override protected def optionNameGuessingSupported: Boolean = false
  override protected def performOptionNameGuessing(): Unit = {
    // noop, no reflection support in Scala-Native
  }

  errorMessageHandler = { message =>
    stderrPrintln(Util.format("[%s] Error: %s", printedName, message))
    exitHandler(1)
  }

}
