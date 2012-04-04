import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._

class ErrorsTest extends FunSuite with ShouldMatchers {
  test("wrong arg type") {
    val opts = Scallop(List("--angels","42"))
      .opt[Int]("angels")
      .verify
    intercept[WrongTypeRequest] {
      opts.get[Double]("angels") should equal (Some(42))
    }
  }

  test("wrong arg type 2") {
    val opts = Scallop(List("--angels","42"))
      .opt[Int]("angels")
      .verify
    intercept[WrongTypeRequest] {
      opts[Double]("angels") should equal (42)
    }
  }
  
  test ("options parse failure") {
    intercept[OptionParseException] { 
      val opts = Scallop(List("42"))
        .verify
    }
  }
  
  test ("long option clash") {
    intercept[IdenticalOptionNames] {
      val opts = Scallop()
        .opt[Int]("ang")
        .opt[Int]("ang")
        .verify
    }
  }

  test ("short option clash") {
    intercept[IdenticalOptionNames] {
      val opts = Scallop()
        .opt[Int]("opt1", short = 'o')
        .opt[Int]("opt2", short = 'o')
        .verify
    }
  }

  test ("unknown short option") {
    intercept[UnknownOption] {
      val opts = Scallop(List("-a"))
        .verify
    }
  }

  test ("unknown long option") {
    intercept[UnknownOption] {
      val opts = Scallop(List("--ang"))
        .verify
    }
  }
  
  test ("option verification failure") {
    intercept[WrongOptionFormat] {
      val opts = Scallop(List("-a","1.2", "-b"))
        .opt[Int]("ang")
        .opt[Boolean]("bang")
        .verify
    }
  }

  test ("required option missing") {
    intercept[RequiredOptionNotFound] {
      val opts = Scallop(List())
        .opt[Int]("ang", required = true)
        .verify
    }
  }
  
  test ("props name clash") {
    intercept[IdenticalOptionNames] {
      val opts = Scallop()
        .props('E')
        .props('E')
        .verify
    }
  }

  test ("opts & props name clash") {
    intercept[IdenticalOptionNames] {
      val opts = Scallop()
        .props('E')
        .opt[Int]("eng", short = 'E')
        .verify
    }
  }

  test ("unknown prop name") {
    intercept[UnknownOption] {
      val opts = Scallop(List("-Eaoeu=aoeu"))
      .verify
    }
  }
  
  test ("unknown option requested") {
    intercept[UnknownOption] {
      val opts = Scallop()
        .verify
      opts[Int]("aoeu")
    }

  }
  
  test ("no option type provided") {
    val opts = Scallop(List("--angels","42"))
      .opt[Int]("angels")
      .verify
    intercept[WrongTypeRequest] {
      opts.get("angels") should equal (Some(42))
    }
  }
  
  test ("excess arguments") {
    intercept[OptionParseException] {
      val opts = Scallop(List("1","2"))
        .trailArg[Int]("first")
        .verify
    }
  }
  
}
