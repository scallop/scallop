package org.rogach.scallop

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class ChoiceOptionsTest extends FunSuite with Matchers with UsefulMatchers {
  throwError.value = true

  test ("choice option") {
    class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
      val letter = choice(Seq("alpha", "beta", "gamma"))
      verify()
    }

    val conf = new Conf(Seq("--letter", "alpha"))
    conf.letter() shouldEqual "alpha"
  }

  test ("wrong choice option argument") {
    class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
      val letter = choice(Seq("alpha", "beta", "gamma"))
      verify()
    }

    expectException(WrongOptionFormat("letter", "alph", "Expected one of: alpha, beta, gamma")) {
      new Conf(Seq("--letter", "alph"))
    }
  }


}
