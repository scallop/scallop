package org.rogach.scallop

object NativeTests {
  def main(args: Array[String]) {
    val conf = new ScallopConf(List("-a","3")) {
      val apples = opt[Int]("apples")
      verify()
    }
    assert(conf.apples() == 3)
  }
}
