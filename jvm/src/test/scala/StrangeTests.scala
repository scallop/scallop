package org.rogach.scallop

import org.rogach.scallop.exceptions._

class StrangeTests extends ScallopTestBase {

  test ("changing printed program name") {
    throwError.withValue(false) {
      overrideColorOutput.withValue(Some(false)) {
        val (out, err, exits) = captureOutputAndExits {
          object Conf extends ScallopConf(Seq()) {
            val apples = trailArg[Int]("beans")
            printedName = "beans"

            verify()
          }
          Conf
        }
        exits shouldBe List(1)
        out shouldBe ""
        err shouldBe "[beans] Error: Required option 'beans' not found\n"
      }
    }
  }

  test ("accessing unverified builder from default option value resolver") {
    throwError.withValue(true) {
      intercept[Help] {
        class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
          appendDefaultToDescription = true
          val protocol = opt[String](default = Some("http"))
          val port = opt[Int](default = protocol() match {
            case "http" => Some(80)
            case _ => None
          })
          verify()
        }

        val conf = new Conf(Seq("--help"))
        conf.port() // to suppress "never used" warning
      }
    }
  }

  test ("default option value resolver accessing other options") {
    class Conf (arguments: Seq[String]) extends ScallopConf(arguments) {
      val protocol = opt[String](name = "protocol", noshort = true, required = false, default = Some("http"))
      val port = opt[Int](name = "port", noshort = true, default = protocol() match {
        case "http" => Some(80)
        case _ => None
      })
      verify()
    }

    val conf = new Conf(Seq())
    conf.protocol() shouldBe "http"
    conf.port() shouldBe 80
  }

}
