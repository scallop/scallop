package org.rogach.scallop

class ScallopException(message:String) extends Exception(message)

class GenScallopException(mess:String) extends ScallopException(mess)
class WrongTypeRequest(mess:String) extends ScallopException(mess)
class OptionParseException(mess:String) extends ScallopException(mess)
class IdenticalOptionNames(mess:String) extends ScallopException(mess)
class UnknownOption(mess:String) extends ScallopException(mess)
class WrongOptionFormat(mess:String) extends ScallopException(mess)
class RequiredOptionNotFound(mess:String) extends ScallopException(mess)

class MajorInternalException extends ScallopException("Oi, something went awfully wrong. Please report a bug to the developer of the library!")
