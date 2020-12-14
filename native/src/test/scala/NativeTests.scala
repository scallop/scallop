package org.rogach.scallop

object NativeTests {
  def main(args: Array[String]): Unit = {
    object Conf extends ScallopConf(List("-a", "3")) {
      val apples = opt[Int]("apples")
      verify()
    }
    assert(Conf.apples() == 3)
    assert(
      Conf.builder.help ==
        """  -a, --apples  <arg>
          |  -h, --help            Show help message""".stripMargin
    )
    println("\u001b[0;32m" + "native test success" + "\u001b[0m")
  }
}
