package org.rogach.scallop

object NativeTests {
  def main(args: Array[String]): Unit = {
    val conf = new ScallopConf(List("-a", "3")) {
      val apples = opt[Int]("apples")
      verify()
    }
    assert(conf.apples() == 3)
    assert(
      conf.builder.help ==
        """  -a, --apples  <arg>
          |  -h, --help            Show help message""".stripMargin
    )
    println("\u001b[0;32m" + "native test success" + "\u001b[0m")
  }
}
