package org.rogach.scallop

/** Contains helper functions to handle differences between different platforms (JVM, Native, JS). */
object Compat {

  def exit(code: Int): Nothing = {
    sys.exit(code)
  }

}
