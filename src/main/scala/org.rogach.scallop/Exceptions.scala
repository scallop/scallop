package org.rogach.scallop.exceptions

/** Parent class for all exceptions thrown from this library. */
abstract class ScallopException(message:String) extends Exception(message)

/** Thrown when the user requests wrong type of option argument from Scallop. */
class WrongTypeRequest(mess:String) extends ScallopException(mess)
/** Thrown when Scallop fails to parse the argument line (usually when there   
    are some problems with trailing args). */
class OptionParseException(mess:String) extends ScallopException(mess)
/** Thrown when several options and/or trailing arguments have identical names
    in definition - making it impossible to distinguish between them. */
class IdenticalOptionNames(mess:String) extends ScallopException(mess)
/** Thrown when user provides Scallop with unknown option name in the arguments
    or requests unknown option result from parser. */
class UnknownOption(mess:String) extends ScallopException(mess)
/** Thrown then arguments to some option do not satisfy that option's
    value converter (it returns Left in such case). */
class WrongOptionFormat(mess:String) extends ScallopException(mess)
/** Thrown when parser failed to find arguments to option (marked as 'required')
    in the input arguments. */
class RequiredOptionNotFound(mess:String) extends ScallopException(mess)
/** Thrown then user tried to extract the value of an option before the call
    to verify in ScallopConf. */
class IncompleteBuildException(mess:String) extends ScallopException(mess)

/** This is a special case of exception - the one you should never see.
    If you actually saw it, there is a sure bug lurking somewhere. In such cases, please
    file a bug on project page! */
class MajorInternalException extends ScallopException("Oi, something went awfully wrong. Please report a bug to the developer of the library!")
