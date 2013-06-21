package org.rogach.scallop

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class OptionDependenciesTest extends FunSuite with ShouldMatchers with UsefulMatchers {
  throwError.value = true

  test("dependsOnAny - success, option is not provided") {
    object Conf extends ScallopConf(List()){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      dependsOnAny(apples, List(bananas, coconuts))
    }
    Conf
  }
  test("dependsOnAny - failure, option is provided, but dependency is not ") {
    object Conf extends ScallopConf(List("-a", "1")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      dependsOnAny(apples, List(bananas, coconuts))
    }
    intercept[ValidationFailure] {
      Conf
    }
  }
  test("dependsOnAny - success, option and dependency are provided") {
    object Conf extends ScallopConf(List("-a","1","-b","2")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      dependsOnAny(apples, List(bananas, coconuts))
    }
    Conf
  }
  test("dependsOnAny - success, option and two dependencies are provided") {
    object Conf extends ScallopConf(List("-a","1","-b","2","-c","3")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      dependsOnAny(apples, List(bananas, coconuts))
    }
    Conf
  }

  test("dependsOnAll - success, option is not provided") {
    object Conf extends ScallopConf(List()){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      dependsOnAll(apples, List(bananas, coconuts))
    }
    Conf
  }
  test("dependsOnAll - failure, option is provided, but no dependencies") {
    object Conf extends ScallopConf(List("-a", "1")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      dependsOnAll(apples, List(bananas, coconuts))
    }
    intercept[ValidationFailure] {
      Conf
    }
  }
  test("dependsOnAll - failure, only one dependency is provided") {
    object Conf extends ScallopConf(List("-a","1","-b","2")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      dependsOnAll(apples, List(bananas, coconuts))
    }
    intercept[ValidationFailure] {
      Conf
    }
  }
  test("dependsOnAll - success, option and two dependencies are provided") {
    object Conf extends ScallopConf(List("-a","1","-b","2","-c","3")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      dependsOnAll(apples, List(bananas, coconuts))
    }
    Conf
  }

  test("conflicts - success, no options are provided") {
    object Conf extends ScallopConf(List()){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      conflicts(apples, List(bananas, coconuts))
    }
    Conf
  }
  test("conflicts - success, only one option is provided") {
    object Conf extends ScallopConf(List("-a", "1")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      conflicts(apples, List(bananas))
    }
    Conf
  }
  test("conflicts - failure, both options are provided") {
    object Conf extends ScallopConf(List("-a","1","-b","2")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      conflicts(apples, List(bananas))
    }
    intercept[ValidationFailure] {
      Conf
    }
  }
  
  test("requireOne - failure, no options are provided") {
    object Conf extends ScallopConf(List()){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      requireOne(apples, bananas)
    }
    intercept[ValidationFailure] {
      Conf
    }
  }
  test("requireOne - success, one is provided") {
    object Conf extends ScallopConf(List("-a", "1")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      requireOne(apples, bananas)
    }
    Conf
  }
  test("requireOne - failure, both options are provided") {
    object Conf extends ScallopConf(List("-a","1","-b","2")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      requireOne(apples, bananas)
    }
    intercept[ValidationFailure] {
      Conf
    }
  }

  test ("option set validation, mutually exclusive options, success") {
    object Conf extends ScallopConf(List("-a","1")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      mutuallyExclusive(apples, bananas)
    }
    Conf
  }

  test ("option set validation, mutually exclusive options, failure") {
    object Conf extends ScallopConf(List("-a", "1", "-b", "2")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      mutuallyExclusive(apples, bananas)
    }
    intercept[ValidationFailure] {
      Conf
    }
  }

  test ("option set validation, codependent options, success") {
    object Conf extends ScallopConf(List("-a","1","-b","2")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      codependent(apples, bananas)
    }
    Conf
  }

  test ("option set validation, codependent options, failure") {
    object Conf extends ScallopConf(List("-a", "1")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      codependent(apples, bananas)
    }
    intercept[ValidationFailure] {
      Conf
    }
  }

  test ("mutually exclusive flag options - validation success") {
    object Conf extends ScallopConf(List("-a")) {
      val apples = opt[Boolean]("apples")
      val bananas = opt[Boolean]("bananas")
      mutuallyExclusive(apples,bananas)
    }
    Conf
  }

  test ("mutually exclusive flag options - validation failure") {
    object Conf extends ScallopConf(List("-a", "-b")) {
      val apples = opt[Boolean]("apples")
      val bananas = opt[Boolean]("bananas")
      mutuallyExclusive(apples,bananas)
    }
    intercept[ValidationFailure] {
      Conf
    }
  }

}
