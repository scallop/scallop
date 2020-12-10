package org.rogach.scallop

class LoadTest extends ScallopTestBase {

  test ("too many options") {
    val N = 100000
    val opts = List.fill(N)(List("-a","1")).flatten
    object Conf extends ScallopConf(opts) {
      val apples = opt[List[Int]]("apples")

      verify()
    }
    Conf.apples() should have size (N)
  }

}
