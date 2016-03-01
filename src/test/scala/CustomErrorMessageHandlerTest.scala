package org.rogach.scallop

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class CustomErrorMessageHandlerTest extends FunSuite with UsefulMatchers {
  throwError.value = false

  case object Err extends Exception

  test ("custom error message handler") {
    intercept[Err.type] {
      new ScallopConf(Seq("-a")) {
        errorMessageHandler = error => throw Err
      }
    }
  }
}
