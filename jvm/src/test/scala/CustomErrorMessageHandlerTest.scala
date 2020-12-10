package org.rogach.scallop

class CustomErrorMessageHandlerTest extends ScallopTestBase {

  case object Err extends Exception

  test ("custom error message handler") {
    throwError.withValue(false) {
      intercept[Err.type] {
        new ScallopConf(Seq("-a")) {
          errorMessageHandler = error => throw Err
          verify()
        }
      }
    }
  }

}
