package org.rogach.scrollop

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scrollop._

class ErrorsTest extends FunSuite with ShouldMatchers {
  test("wrong arg type") {
    val opts = Scrollop(List("--angels","42"))
      .opt[Int]("angels")
      .verify
    intercept[WrongTypeRequest] {
      opts.get[Double]("angels") should equal (Some(42))
    }
  }

  test("wrong arg type 2") {
    val opts = Scrollop(List("--angels","42"))
      .opt[Int]("angels")
      .verify
    intercept[WrongTypeRequest] {
      opts[Double]("angels") should equal (42)
    }
  }
  
  test ("options parse failure") {
    intercept[OptionParseException] { 
      val opts = Scrollop(List("42"))
        .verify
    }
  }
  
  test ("long option clash") {
    intercept[IdenticalOptionNames] {
      val opts = Scrollop()
        .opt[Int]("ang")
        .opt[Int]("ang")
        .verify
    }
  }

  test ("short option clash") {
    intercept[IdenticalOptionNames] {
      val opts = Scrollop()
        .opt[Int]("opt1", short = 'o')
        .opt[Int]("opt2", short = 'o')
        .verify
    }
  }

  test ("unknown short option") {
    intercept[UnknownOption] {
      val opts = Scrollop(List("-a"))
        .verify
    }
  }

  test ("unknown long option") {
    intercept[UnknownOption] {
      val opts = Scrollop(List("--ang"))
        .verify
    }
  }
  
  test ("option verification failure") {
    intercept[WrongOptionFormat] {
      val opts = Scrollop(List("-a","1.2"))
        .opt[Int]("ang")
        .verify
    }
  }

  test ("required option missing") {
    intercept[RequiredOptionNotFound] {
      val opts = Scrollop(List())
        .opt[Int]("ang", required = true)
        .verify
    }
  }
  
  test ("props name clash") {
    intercept[IdenticalOptionNames] {
      val opts = Scrollop()
        .props('E')
        .props('E')
        .verify
    }
  }

  test ("opts & props name clash") {
    intercept[IdenticalOptionNames] {
      val opts = Scrollop()
        .props('E')
        .opt[Int]("eng", short = 'E')
        .verify
    }
  }

  test ("unknown prop name") {
    intercept[UnknownOption] {
      val opts = Scrollop(List("-Eaoeu=aoeu"))
      .verify
    }
  }
  
  test ("unknown option requested") {
    intercept[UnknownOption] {
      val opts = Scrollop()
        .verify
      opts[Int]("aoeu")
    }

  }
  
}
