package org.rogach.scallop

object Compat {

  def exit(code: Int): Nothing = {
    sys.exit(code)
  }

}
