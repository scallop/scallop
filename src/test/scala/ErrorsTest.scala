package org.rogach.scallop

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class ErrorsTest extends FunSuite with Matchers with UsefulMatchers {
  throwError.value = true

  test ("options parse failure") {
    expectException(ExcessArguments(Seq("42"))) {
      val opts = Scallop(List("42"))
        .verify
    }
  }

  test ("long option clash") {
    expectException(IdenticalOptionNames("Option identifier 'ang' is not unique")) {
      val opts = Scallop()
        .opt[Int]("ang")
        .opt[Int]("ang")
        .verify
    }
  }

  test ("short option clash") {
    expectException(IdenticalOptionNames("Short option name 'o' is not unique")) {
      val opts = Scallop()
        .opt[Int]("opt1", short = 'o')
        .opt[Int]("opt2", short = 'o')
        .verify
    }
  }

  test ("unknown short option") {
    expectException(UnknownOption("a")) {
      val opts = Scallop(List("-a"))
        .verify
    }
  }

  test ("unknown long option") {
    expectException(UnknownOption("ang")) {
      val opts = Scallop(List("--ang"))
        .verify
    }
  }

  test ("option verification failure") {
    expectException(WrongOptionFormat("ang", "1.2", "bad Int value")) {
      val opts = Scallop(List("-a","1.2", "-b"))
        .opt[Int]("ang")
        .opt[Boolean]("bang")
        .verify
    }
  }

  test ("required option missing") {
    expectException(RequiredOptionNotFound("ang")) {
      val opts = Scallop(List())
        .opt[Int]("ang", required = true)
        .verify
    }
  }

  test ("props name clash") {
    expectException(IdenticalOptionNames("Option identifier 'E' is not unique")) {
      val opts = Scallop()
        .props[String]('E')
        .props[String]('E')
        .verify
    }
  }

  test ("opts & props name clash") {
    expectException(IdenticalOptionNames("Short option name 'E' is not unique")) {
      val opts = Scallop()
        .props[String]('E')
        .opt[Int]("eng", short = 'E')
        .verify
    }
  }

  test ("unknown prop name") {
    expectException(UnknownOption("E")) {
      val opts = Scallop(List("-Eaoeu=aoeu"))
      .verify
    }
  }

  test ("unknown option requested") {
    expectException(UnknownOption("aoeu")) {
      val opts = Scallop()
        .verify
      opts("aoeu")
    }
  }

  test ("excess arguments") {
    expectException(ExcessArguments(Seq("2"))) {
      val opts = Scallop(List("1","2"))
        .trailArg[Int]("first")
        .verify
    }
  }

  test ("validation failure") {
    expectException(ValidationFailure("Validation failure for 'apples' option parameters: 1")) {
      val opts = Scallop(List("-a","1"))
        .opt[Int]("apples", validate = (0>))
        .verify
    }
  }

  test ("option identifier clash") {
    expectException(IdenticalOptionNames("Option identifier 'tasks' is not unique")) {
      val opts = Scallop(Nil)
        .opt[Boolean]("tasks")
        .trailArg[List[String]]("tasks")
        .verify
    }
  }

  test ("subcommand parse failure") {
    expectException(WrongOptionFormat("apples", "a", "wrong arguments format")) {
      new  ScallopConf(Seq("tree", "a")) {
        val tree = new Subcommand("tree") {
          val apples = trailArg[List[Int]]()
        }
        addSubcommand(tree)

        verify()
      }
    }
  }

  test ("parse failure on half-provided single-arg option") {
    expectException(WrongOptionFormat("apples", "", "you should provide exactly one argument")) {
      new ScallopConf(Seq("-a")) {
        val apples = opt[Int]("apples")
        verify()
      }
    }
  }
}
