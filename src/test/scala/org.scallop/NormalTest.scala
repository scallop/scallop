package org.rogach.scallop

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._

class NormalTest extends FunSuite with ShouldMatchers {
  
  test ("main") {
val opts = Scallop(List("-d","--num-limbs","1"))
  .version("test 1.2.3 (c) 2012 Mr S") // --version option is provided for you
                                       // in "verify" stage it would print this message and exit
  .banner("""Usage: test [OPTION]...
            |test is an awesome program, which does something funny      
            |Options:
            |""".stripMargin) // --help is also provided
                              //  will also exit after printing version, banner, and options usage
  .opt[Boolean]("donkey", descr = "use donkey mode") // simple flag option
  .opt("monkeys", default = Some(2), short = 'm') // you can add the default option
                                                  // the type will be inferred
  .opt[Int]("num-limbs", 'k', 
    "number of libms", required = true) // you can override the default short-option character
  .opt[List[Double]]("params") // default converters are provided for all primitives
                               //and for lists of primitives
  .props('D',"some key-value pairs")
  .args(List("-Dalpha=1","-D","betta=2","gamma=3")) // you can add parameters a bit later
  .verify
  
opts.get[Boolean]("donkey") should equal (Some(true))
opts[Int]("monkeys") should equal (2)
opts[Int]("num-limbs") should equal (1)
opts.prop('D',"alpha") should equal (Some("1"))
opts.prop('E',"gamma") should equal (None)
intercept[WrongTypeRequest] {
  opts[Double]("monkeys") // this will throw an exception at runtime
                          // because the wrong type is requested
}

println(opts.help)
//opts.args(List("--help")).verify
  }
  
  test ("no values") {
    Scallop().verify
    Scallop(Array[String]()).verify
    Scallop(List()).verify
  }
  
  test ("long flag") {
    val opts = Scallop(List("--angel"))
      .opt[Boolean]("angel")
      .verify
    opts[Boolean]("angel") should equal (true)
  }
  
  test ("short flag, explicit name") {
    val opts = Scallop(List("-a"))
      .opt[Boolean]("angel", short = 'a')
      .verify
    opts[Boolean]("angel") should equal (true)
  }

  test ("short flag, implicit name") {
    val opts = Scallop(List("-a"))
      .opt[Boolean]("angel")
      .verify
    opts[Boolean]("angel") should equal (true)
  }
  
  test ("two short flags, implicit name") {
    val opts = Scallop(List("-a"))
      .opt[Boolean]("angel")
      .opt[Boolean]("baboon")
      .verify
    opts[Boolean]("angel") should equal (true)
    opts[Boolean]("baboon") should equal (false)
  }
  
  test ("two short flags, implicit name, required value") {
    val opts = Scallop(List("-a"))
      .opt[Boolean]("angel", required = true)
      .opt[Boolean]("baboon")
      .verify
    opts[Boolean]("angel") should equal (true)
    opts[Boolean]("baboon") should equal (false)
  }
 
  test ("one missing int, short opt") {
    val opts = Scallop(List())
      .opt[Int]("angels")
      .verify
    opts.get[Int]("angels") should equal (None)
  }

  test ("one int, short opt") {
    val opts = Scallop(List("-a","42"))
      .opt[Int]("angels")
      .verify
    opts.get[Int]("angels") should equal (Some(42))
  }

  test ("one int, long opt") {
    val opts = Scallop(List("--angels","42"))
      .opt[Int]("angels")
      .verify
    opts.get[Int]("angels") should equal (Some(42))
  }

  test ("one short, long opt") {
    val opts = Scallop(List("--angels","42"))
      .opt[Short]("angels")
      .verify
    opts.get[Short]("angels") should equal (Some(42))
  }

  test ("one byte, long opt") {
    val opts = Scallop(List("--angels","42"))
      .opt[Byte]("angels")
      .verify
    opts.get[Byte]("angels") should equal (Some(42))
  }
  
  test ("one double, long opt") {
    val opts = Scallop(List("--angels","42"))
      .opt[Double]("angels")
      .verify
    opts.get[Double]("angels") should equal (Some(42))
  }

  test ("one string, long opt") {
    val opts = Scallop(List("--angels","aoeu"))
      .opt[String]("angels")(stringConverter, implicitly[Manifest[String]])
      .verify
    opts.get[String]("angels") should equal (Some("aoeu"))
  }

  
  test ("list of ints, long opt") {
    val opts = Scallop(List("--angels","42","12","345"))
      .opt[List[Int]]("angels")
      .verify
    opts.get[List[Int]]("angels") should equal (Some(List(42,12,345)))
  }

  test ("list of doubles, long opt") {
    val opts = Scallop(List("--angels","42.0","12","345e0"))
      .opt[List[Double]]("angels")
      .verify
    opts.get[List[Double]]("angels") should equal (Some(List(42.0,12.0,345.0)))
  }

  test ("default value") {
    val opts = Scallop(List())
      .opt("ang", default = Some(42), required = true)
      .verify
    opts[Int]("ang") should equal (42)
  }

  test ("additional args") {
    val opts = Scallop(List("-a","5"))
      .opt[List[Int]]("ang")
      .args(List("-a","10"))
      .verify
    opts[List[Int]]("ang") should equal (List(5,10))
  }
  
  // properties testing
  
  test ("no value") {
    val opts = Scallop()
      .props('D')
      .verify
    opts.prop('D',"aoeu") should equal (None)
  }

  test ("simle value") {
    val opts = Scallop(List("-Daoeu=htns"))
      .props('D')
      .verify
    opts.prop('D',"aoeu") should equal (Some("htns"))
  }

  test ("one plain prop") {
    val opts = Scallop(List("-D","aoeu=htns"))
      .props('D')
      .verify
    opts.prop('D',"aoeu") should equal (Some("htns"))
  }

  test ("two plain props") {
    val opts = Scallop(List("-D", "aoeu=htns", "qjk=gcr"))
      .props('D')
      .verify
    opts.prop('D',"aoeu") should equal (Some("htns"))
    opts.prop('D',"qjk") should equal (Some("gcr"))
  }
  
  test ("opt implicit name clash with prop name") {
    val opts = Scallop(List("-D", "aoeu=htn"))
      .props('D')
      .opt[String]("Dark")
      .verify
    opts.get[String]("Dark") should equal (None)
  }
  
}
