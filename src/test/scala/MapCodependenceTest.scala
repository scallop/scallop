package org.rogach.scallop

import org.scalatest.FunSuite
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class MapCodependenceTest extends FunSuite with UsefulMatchers {

  class TestConf(args: Seq[String]) extends ScallopConf(args) {
    val apples = opt[Boolean]("apples")
    val bananas = opt[Map[String,String]]("bananas")
    codependent(apples, bananas)
  }

  test("failing codependency including unsupplied map") {
    expectException(ValidationFailure("Either all or none of the following options should be supplied, because they are co-dependent: apples, bananas")) {
      new TestConf(Seq("--apples"))
    }
  }

  test("succeeding codependency including supplied map") {
    new TestConf(Seq("--apples", "--bananas", "zoop=zop"))
  }

  test("succeeding codependency including unsupplied map") {
    new TestConf(Seq.empty)
  }

}
