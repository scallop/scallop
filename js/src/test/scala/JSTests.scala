package org.rogach.scallop

import scala.scalajs.js

object Main extends js.JSApp {
  def main(): Unit = {
    val conf = new ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify()
    }
    assert(conf.apples() == 3)
  }
}
