package org.rogach.scallop

/** Contains helper functions to handle differences between different platforms (JVM, Native, JS). */
object Compat {

  @volatile var trapExits: Boolean = false
  @volatile var exitStatuses: List[Int] = List[Int]()

  def exit(code: Int): Nothing = {
    if (trapExits) {
      exitStatuses ::= code
      throw new RuntimeException("trapped exit")
    } else {
      sys.exit(code)
    }
  }

}
