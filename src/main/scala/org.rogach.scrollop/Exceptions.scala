package org.rogach.scrollop

class ScrollopException(message:String) extends Exception(message)

class GenScrollopException(mess:String) extends ScrollopException(mess)
class WrongTypeRequest(mess:String) extends ScrollopException(mess)
class OptionParseException(mess:String) extends ScrollopException(mess)
class IdenticalOptionNames(mess:String) extends ScrollopException(mess)
class UnknownOption(mess:String) extends ScrollopException(mess)
class WrongOptionFormat(mess:String) extends ScrollopException(mess)
class RequiredOptionNotFound(mess:String) extends ScrollopException(mess)
