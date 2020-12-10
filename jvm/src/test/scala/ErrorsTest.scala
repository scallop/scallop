package org.rogach.scallop

import org.rogach.scallop.exceptions._

class ErrorsTest extends ScallopTestBase {

  test ("options parse failure") {
    expectException(ExcessArguments(Seq("42"))) {
      object Conf extends ScallopConf(Seq("42")) {
        verify()
      }
      Conf
    }
  }

  test ("long option clash") {
    expectException(IdenticalOptionNames("Option identifier 'ang' is not unique")) {
      object Conf extends ScallopConf(Seq()) {
        val ang1 = opt[Int]("ang")
        val ang2 = opt[Int]("ang")
        verify()
      }
      Conf
    }
  }

  test ("short option clash") {
    expectException(IdenticalOptionNames("Short option name 'o' is not unique")) {
      object Conf extends ScallopConf(Seq()) {
        val opt1 = opt[Int]("opt1", short = 'o')
        val opt2 = opt[Int]("opt2", short = 'o')
        verify()
      }
      Conf
    }
  }

  test ("unknown short option") {
    expectException(UnknownOption("a")) {
      object Conf extends ScallopConf(Seq("-a")) {
        verify()
      }
      Conf
    }
  }

  test ("unknown long option") {
    expectException(UnknownOption("ang")) {
      object Conf extends ScallopConf(Seq("--ang")) {
        verify()
      }
      Conf
    }
  }

  test ("option verification failure") {
    expectException(WrongOptionFormat("ang", "1.2", "bad Int value")) {
      object Conf extends ScallopConf(Seq("-a", "1.2", "-b")) {
        val ang = opt[Int]("ang")
        val bang = opt[Boolean]("bang")
        verify()
      }
      Conf
    }
  }

  test ("required option missing") {
    expectException(RequiredOptionNotFound("ang")) {
      object Conf extends ScallopConf(Seq()) {
        val ang = opt[Int]("ang", required = true)
        verify()
      }
      Conf
    }
  }

  test ("props name clash") {
    expectException(IdenticalOptionNames("Option identifier 'E' is not unique")) {
      object Conf extends ScallopConf(Seq()) {
        val e1 = props[String]('E')
        val e2 = props[String]('E')
        verify()
      }
      Conf
    }
  }

  test ("opts & props name clash") {
    expectException(IdenticalOptionNames("Short option name 'E' is not unique")) {
      object Conf extends ScallopConf(Seq()) {
        val e = props[String]('E')
        val eng = opt[Int]("eng", short = 'E')
        verify()
      }
      Conf
    }
  }

  test ("unknown prop name") {
    expectException(UnknownOption("E")) {
      object Conf extends ScallopConf(Seq("-Eaoeu=aoeu")) {
        verify()
      }
      Conf
    }
  }

  test ("excess arguments") {
    expectException(ExcessArguments(Seq("2"))) {
      object Conf extends ScallopConf(Seq("1","2")) {
        val first = trailArg[Int]("first")
        verify()
      }
      Conf
    }
  }

  test ("validation failure") {
    expectException(ValidationFailure("Validation failure for 'apples' option parameters: 1")) {
      object Conf extends ScallopConf(Seq("-a","1")) {
        val apples = opt[Int]("apples", validate = (_ < 0))
        verify()
      }
      Conf
    }
  }

  test ("option identifier clash") {
    expectException(IdenticalOptionNames("Option identifier 'tasks' is not unique")) {
      object Conf extends ScallopConf(Seq()) {
        val tasks = opt[Boolean]("tasks")
        val tasks2 = trailArg[List[String]]("tasks")
        verify()
      }
      Conf
    }
  }

  test ("subcommand parse failure") {
    expectException(WrongOptionFormat("apples", "a", "java.lang.NumberFormatException: For input string: \"a\"")) {
      object Conf extends ScallopConf(Seq("tree", "a")) {
        object tree extends Subcommand("tree") {
          val apples = trailArg[List[Int]]("apples")
        }
        addSubcommand(tree)

        verify()
      }
      Conf
    }
  }

  test ("parse failure on half-provided single-arg option") {
    expectException(WrongOptionFormat("apples", "", "you should provide exactly one argument")) {
      object Conf extends ScallopConf(Seq("-a")) {
        val apples = opt[Int]("apples")
        verify()
      }
      Conf
    }
  }

}
