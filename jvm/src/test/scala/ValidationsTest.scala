package org.rogach.scallop

import org.rogach.scallop.exceptions._

class ValidationsTest extends ScallopTestBase {

  test ("one-arg option validation") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      val apples = opt[Int]("apples", validate = (0 < _))
      verify()
    }
    Conf.apples() shouldBe 1
  }

  test ("failing validation") {
    expectException(ValidationFailure("Validation failure for 'apples' option parameters: 1")) {
      object Conf extends ScallopConf(List("-a","1")) {
        val apples = opt[Int]("apples", validate = (_ < 0))

      }
      Conf.verify()
    }
  }

  test ("custom validation - success") {
    object Conf extends ScallopConf(List("-a","14","-b","3")) {
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      validate (apples, bananas) { (a,b) =>
        if (b > 0 && a % 7 == 0) Right(())
        else Left("Something is wrong with composition :)")
      }

    }
    Conf.verify()
  }

  test ("custom validation - failure") {
    expectException(ValidationFailure("Something is wrong with composition :)")) {
      object Conf extends ScallopConf(List("-a","15","-b","3")) {
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        validate (apples, bananas) { (a,b) =>
          if (b > 0 && a % 7 == 0) Right(())
          else Left("Something is wrong with composition :)")
        }
      }
      Conf.verify()
    }
  }

  test ("custom opt validation - success") {
    object Conf extends ScallopConf(List("-a", "14")) {
      val apples = opt[Int]()
      val bananas = opt[Int]()
      validateOpt (apples, bananas) {
        case (Some(a), None) => Right(())
        case _ => Left("err")
      }
    }
    Conf.verify()
  }

  test ("custom opt validation - failure") {
    expectException(ValidationFailure("err")) {
      object Conf extends ScallopConf(List("-a", "14", "-b", "4")) {
        val apples = opt[Int]()
        val bananas = opt[Int]()
        validateOpt (apples, bananas) {
          case (Some(a), None) => Right(())
          case _ => Left("err")
        }
      }
      Conf.verify()
    }
  }

  test ("validate should fall back to default values if they are present") {
    object Conf extends ScallopConf(List("--start", "7")) {
      val start = opt[Int]("start", default = Some(1))
      val end = opt[Int]("end", default = Some(10))
      validate (start, end) { (s,e) =>
        if (s < e) Right(())
        else Left("Start must be before end")
      }
    }
    Conf.verify()
  }

  test ("validate should fall back to default values if they are present - error case") {
    expectException(ValidationFailure("Start must be before end")) {
      object Conf extends ScallopConf(List("--start", "7")) {
        val start = opt[Int]("start", default = Some(1))
        val end = opt[Int]("end", default = Some(5))
        validate (start, end) { (s,e) =>
          if (s < e) Right(())
          else Left("Start must be before end")
        }
      }
      Conf.verify()
    }
  }

  test ("validate function should not run if all options are not provided") {
    object Conf extends ScallopConf(List()) {
      val start = opt[Int]("start")
      val end = opt[Int]("end")
      validate (start, end) { (s,e) =>
        if (s <= e) Right(())
        else Left("Start must be before end")
      }

    }
    Conf.verify()
  }

}
