package org.rogach.scallop.exceptions

/** Parent trait for all not-positive results that can be resulted from this library (including help and exits) */
sealed trait ScallopResult

/** Thrown when user requested help output (via "--help") */
case class Help(command: String) extends Throwable with ScallopResult
/** Thrown when user requested version printout (via "--version") */
case object Version extends Throwable with ScallopResult
/** Extractor object, for matching on both Help and Version results. */
object Exit {
  def unapply(r: ScallopResult) = r match {
    case Help(_) => true
    case Version => true
    case _ => false
  }
}

/** Parent class for all exceptions thrown from this library. */
abstract class ScallopException(val message:String) extends Exception(message) with ScallopResult
/** Extractor object, for matching on all exceptions. */
object ScallopException {
  def unapply(e: ScallopException): Option[String] = {
    Some(e.message)
  }
}

/** Thrown when the user requests wrong type of option argument from Scallop. */
case class WrongTypeRequest(requested: Manifest[_], required: Manifest[_])
  extends ScallopException("Requested '%s' instead of '%s'" format (requested, required))
/** Thrown when Scallop fails to parse the argument line (usually when there
    are some problems with trailing args). */
case class TrailingArgsParseException(failedArgs: Seq[String])
  extends ScallopException("Failed to parse the trailing argument list: '%s'" format failedArgs.mkString(" "))
/** Thrown when several options and/or trailing arguments have identical names
    in definition - making it impossible to distinguish between them. */
case class IdenticalOptionNames(mess:String) extends ScallopException(mess)
/** Thrown when user provides Scallop with unknown option name in the arguments
    or requests unknown option result from parser. */
case class UnknownOption(optionName: String)
  extends ScallopException("Unknown option '%s'" format optionName)
/** Thrown when arguments to some option do not satisfy that option's
    value converter (it returns Left in such case). */
case class WrongOptionFormat(optionName: String, args: String)
  extends ScallopException("Wrong format for option '%s': '%s'" format (optionName, args))
/** Thrown when parser failed to find arguments to option (marked as 'required')
    in the input arguments. */
case class RequiredOptionNotFound(name: String)
  extends ScallopException("Required option '%s' not found" format name)
/** Thrown when user tried to extract the value of an option before the call
    to verify in ScallopConf. */
case class IncompleteBuildException()
  extends ScallopException("It seems you tried to get option value before you constructed all options (maybe you forgot to call .verify method?). Please, move all extraction of values to after 'verify' method in ScallopConf.")
/** Thrown when user tried to create an option with some illegal parameters
    - for example, with digit as the first character in opton name. */
case class IllegalOptionParameters(mess:String) extends ScallopException(mess)
/** Thrown when the validation failed on some option. */
case class ValidationFailure(mess: String) extends ScallopException(mess)
/** Trown when some validation for option set failed. */
case class OptionSetValidationFailure(mess:String) extends ScallopException(mess)

/** This is a special case of exception - the one you should never see.
    If you actually saw it, there is a sure bug lurking somewhere. In such cases, please
    file a bug on project page! */
case class MajorInternalException() extends ScallopException("Oi, something went awfully wrong. Please report a bug to the developer of the library!")
