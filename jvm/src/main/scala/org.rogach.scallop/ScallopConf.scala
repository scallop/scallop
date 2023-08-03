package org.rogach.scallop

import scala.collection.{Seq => CSeq}
import org.rogach.scallop.exceptions.OptionNameGuessingException

/** Base class for CLI parsers. */
abstract class ScallopConf(
  args: CSeq[String] = Nil,
  commandNameAndAliases: Seq[String] = Nil
) extends ScallopConfBase(args, commandNameAndAliases) {

  // machinery to support option name guessing
  override protected def optionNameGuessingSupported: Boolean = true
  /** If true, scallop would try to guess missing option names from the names of their fields. */
  def guessOptionName = _guessOptionName
  /** If set to true, scallop would try to guess missing option names from the names of their fields. */
  def guessOptionName_=(v: Boolean): Unit = { _guessOptionName = v }

  override protected def performOptionNameGuessing(): Unit = {
    try {
      val methodsAndOptions =
        this.getClass.getMethods
        .filterNot(classOf[ScallopConf].getMethods.toSet)
        .filterNot(_.getName.endsWith("$eq"))
        .filterNot(_.getName.endsWith("$outer"))
        .filter(_.getReturnType == classOf[ScallopOption[_]])
        .filter(_.getParameterTypes.isEmpty)
        .map { m =>
          val wasAccessible = m.isAccessible()
          if (!wasAccessible) {
            m.setAccessible(true)
          }
          val opt = m.invoke(this).asInstanceOf[ScallopOption[_]]
          if (!wasAccessible) {
            m.setAccessible(false)
          }
          (m, opt)
        }
        .filter(_._2.name.contains("\t"))
        .sortBy(-_._2._transformCount)

      val optionMap = methodsAndOptions.reverse.flatMap { case (m, opt) =>
        val newName =
          m.getName
          .flatMap(c => if (c.isUpper) Seq('-', c.toLower) else Seq(c))
          .mkString
          .replace("$tilde", "~")
          .replace("$eq", "=")
          .replace("$less", "<")
          .replace("$greater", ">")
          .replace("$bang", "!")
          .replace("$hash", "#")
          .replace("$percent", "%")
          .replace("$up", "^")
          .replace("$amp", "&")
          .replace("$bar", "|")
          .replace("$times", "*")
          .replace("$div", "/")
          .replace("$plus", "+")
          .replace("$minus", "-")
          .replace("$colon", ":")
          .replace("$qmark", "?")
          .replace("$at", "@")

        // the old, generated version of name, without prefixes from parent builders
        val shortGenName = '\t' +: opt.name.reverse.takeWhile(_ != '\t').reverse

        opt.cliOption match {
          case Some(cliOption) if cliOption.name == shortGenName =>
            opt._name = () => newName
            Some((cliOption, cliOption match {
              case o: SimpleOption => o.copy(name = newName)
              case o: TrailingArgsOption => o.copy(name = newName)
              case o: ToggleOption => o.copy(name = newName)
              case o: NumberArgOption => o.copy(name = newName)
              case _ => cliOption
            }))
          case _ =>
            Nil
        }
      }.toMap

      editBuilder(e => e.copy(
        opts = e.opts.map(o => optionMap.getOrElse(o, o)),
        mainOptions = e.mainOptions.map(o => optionMap.getOrElse(o, o)),
        optionGroups = e.optionGroups.map(t => t.copy(_2 = t._2.map(o => optionMap.getOrElse(o, o))))
      ))
    } catch {
      case e: Exception =>
        throw OptionNameGuessingException(e)
    }
  }

  errorMessageHandler = { message =>
    if (overrideColorOutput.value.getOrElse(System.console() != null)) {
      stderrPrintln(String.format("[\u001b[31m%s\u001b[0m] Error: %s", printedName, message))
    } else {
      // no colors on output
      stderrPrintln(String.format("[%s] Error: %s", printedName, message))
    }
    exitHandler(1)
  }

}
