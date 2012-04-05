package org.rogach.scallop

/** A class to hold a reference to not-yet-computed option values
  * @param fn source of the actual value 
  */
class ScallopOption[A](fn: => Option[A]) {
  /** Retreive the underlying value as an option */
  def get = fn
  /** Retreive the underlying value. Use only if you are completely sure that there is a value. */
  def apply() = fn.get
}
