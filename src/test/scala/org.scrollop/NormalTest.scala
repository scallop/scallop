package org.rogach.scrollop

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scrollop._

class NormalTest extends FunSuite with ShouldMatchers {
  
  test ("main") {
    val opts = Scrollop(List("-d","--num-limbs","1"))
      .version("test 1.2.3 (c) 2012 Mr Placeholder") // --version option is provided for you, in "verify" stage it would print this message and exit
      .banner("""Usage: test [OPTION]...
                |test is an awesome program, which does something funny      
                |Options:
                |""".stripMargin) // --help is also provided, will also exit after printing version, banner, and options usage
      .opt[Boolean]("donkey", descr = "use donkey mode") // simple flag option
      .opt("monkey", default = Some(true)) // you can add the default option, and the type will be inferred
      .opt[Int]("num-limbs", 'k', "number of libms", required = true) // you can override the default short-option character
      .opt[List[Double]]("params") // default converters are provided for all primitives, and for lists of primitives
      .verify

  }
  
  test ("no values") {
    Scrollop().verify
    Scrollop(Array[String]()).verify
    Scrollop(List()).verify
  }
  
  test ("long flag") {
    val opts = Scrollop(List("--angel"))
      .opt[Boolean]("angel")
      .verify
    opts[Boolean]("angel") should equal (true)
  }
  
  test ("short flag, explicit name") {
    val opts = Scrollop(List("-a"))
      .opt[Boolean]("angel", short = 'a')
      .verify
    opts[Boolean]("angel") should equal (true)
  }

  test ("short flag, implicit name") {
    val opts = Scrollop(List("-a"))
      .opt[Boolean]("angel")
      .verify
    opts[Boolean]("angel") should equal (true)
  }
  
  test ("two short flags, implicit name") {
    val opts = Scrollop(List("-a"))
      .opt[Boolean]("angel")
      .opt[Boolean]("baboon")
      .verify
    opts[Boolean]("angel") should equal (true)
    opts[Boolean]("baboon") should equal (false)
  }
  
  test ("two short flags, implicit name, required value") {
    val opts = Scrollop(List("-a"))
      .opt[Boolean]("angel", required = true)
      .opt[Boolean]("baboon")
      .verify
    opts[Boolean]("angel") should equal (true)
    opts[Boolean]("baboon") should equal (false)
  }
 
  test ("one missing int, short opt") {
    val opts = Scrollop(List())
      .opt[Int]("angels")
      .verify
    opts.get[Int]("angels") should equal (None)
  }

  test ("one int, short opt") {
    val opts = Scrollop(List("-a","42"))
      .opt[Int]("angels")
      .verify
    opts.get[Int]("angels") should equal (Some(42))
  }

  test ("one int, long opt") {
    val opts = Scrollop(List("--angels","42"))
      .opt[Int]("angels")
      .verify
    opts.get[Int]("angels") should equal (Some(42))
  }

  test ("one short, long opt") {
    val opts = Scrollop(List("--angels","42"))
      .opt[Short]("angels")
      .verify
    opts.get[Short]("angels") should equal (Some(42))
  }

  test ("one byte, long opt") {
    val opts = Scrollop(List("--angels","42"))
      .opt[Byte]("angels")
      .verify
    opts.get[Byte]("angels") should equal (Some(42))
  }
  
  test ("one double, long opt") {
    val opts = Scrollop(List("--angels","42"))
      .opt[Double]("angels")
      .verify
    opts.get[Double]("angels") should equal (Some(42))
  }

  test ("list of ints, long opt") {
    val opts = Scrollop(List("--angels","42","12","345"))
      .opt[List[Int]]("angels")
      .verify
    opts.get[List[Int]]("angels") should equal (Some(List(42,12,345)))
  }

  test ("list of doubles, long opt") {
    val opts = Scrollop(List("--angels","42.0","12","345e0"))
      .opt[List[Double]]("angels")
      .verify
    opts.get[List[Double]]("angels") should equal (Some(List(42.0,12.0,345.0)))
  }

  test ("default value") {
    val opts = Scrollop(List())
      .opt("ang", default = Some(42), required = true)
      .verify
    opts[Int]("ang") should equal (42)
  }
  
  test ("additional args") {
    val opts = Scrollop(List("-a","5"))
      .opt[List[Int]]("ang")
      .args(List("-a","10"))
      .verify
    opts[List[Int]]("ang") should equal (List(5,10))
  }
//  test("") { Thread.sleep(10) }
}
