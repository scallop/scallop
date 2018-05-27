package org.rogach.scallop

abstract class ScallopConf(
  args: Seq[String] = Nil,
  commandNameAndAliases: Seq[String] = Nil
) extends ScallopConfBase(args, commandNameAndAliases) {

  override protected def guessOptionNameDefault: Boolean = false
  override protected def performOptionNameGuessing(): Unit = {}

  errorMessageHandler = { message =>
    Console.err.println(Util.format("[%s] Error: %s", printedName, message))
    sys.exit(1)
  }

}
