package org.rogach.scallop

object Compat {

  def exit(code: Int): Nothing = {
    scalajs.js.Dynamic.global.process.exit(code)
    throw new Throwable()
  }

}
