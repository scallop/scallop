package org.rogach.scallop

import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers

class NoshortDefaultValueTest extends UsefulMatchers with CapturingTest with Matchers with Inspectors {

  test("noshort default value via ScallopConf") {

    case class NoshortConf(initialNoshort: Boolean, reassignedNoshort: Boolean) extends ScallopConf(List("-a", "x", "-b", "x", "-c", "-d")) {
      noshort = initialNoshort // for all subsequent options, set global default for noshort to initialNoshort
      val a1 = opt[String]()
      val a2 = opt[String](noshort = false)
      val b1 = choice(choices = Seq("x"))
      val b2 = choice(choices = Seq("x"), noshort = false)
      noshort = reassignedNoshort // for all subsequent options, set global default for noshort to reassignedNoshort
      val c1 = tally()
      val c2 = tally(noshort = false)
      val d1 = toggle()
      val d2 = toggle(noshort = false)
      verify()
    }

    forAll(List((false, false), (false, true), (true, false), (true, true))) { case (initialNoshort, reassignedNoshort) => {
      val conf = NoshortConf(initialNoshort, reassignedNoshort)
      conf.a1.isSupplied shouldBe !initialNoshort
      conf.a2.isSupplied shouldBe initialNoshort
      conf.b1.isSupplied shouldBe !initialNoshort
      conf.b2.isSupplied shouldBe initialNoshort
      conf.c1.isSupplied shouldBe !reassignedNoshort
      conf.c2.isSupplied shouldBe reassignedNoshort
      conf.d1.isSupplied shouldBe !reassignedNoshort
      conf.d2.isSupplied shouldBe reassignedNoshort
    }}
  }

  test("noshort default value via Scallop") {
    forAll(List(false, true)) { noshort => {
      val conf = Scallop(Seq("-a", "x", "-b"), noshort = noshort)
        .opt[String]("a1")
        .opt[String]("a2", noshort = false)
        .toggle("b1")
        .toggle("b2", noshort = false)
        .verify

      conf.isSupplied("a1") shouldBe !noshort
      conf.isSupplied("a2") shouldBe noshort
      conf.isSupplied("b1") shouldBe !noshort
      conf.isSupplied("b2") shouldBe noshort
    }}
  }
}
