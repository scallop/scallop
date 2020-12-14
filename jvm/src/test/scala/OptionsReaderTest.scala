package org.rogach.scallop

import org.rogach.scallop.tokenize._

class OptionsReaderTest extends ScallopTestBase {

  test ("reading options from stdin") {
    withInput("-a 3\n-b 5") {
      object Conf extends ScallopConf(List("@--")) {
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")

        verify()
      }
      Conf.apples() shouldBe 3
      Conf.bananas() shouldBe 5
    }
  }

  test ("reading options from file") {
    object Conf extends ScallopConf(List("@src/test/resources/opts.txt")) {
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")

      verify()
    }
    Conf.apples() shouldBe 3
    Conf.bananas() shouldBe 5
  }

  test ("reading options from file, with spaces in arguments") {
    object Conf extends ScallopConf(List("@src/test/resources/opts-with-spaces.txt")) {
      val foo = opt[String]("foo")
      val bar = opt[String]("bar")
      val baz = opt[String]("baz")
      val coo = opt[String]("coo")
      val escapedSingleQuote = opt[String]("escaped-single-quote")
      verify()
    }
    Conf.foo() shouldBe "A \"B\" C"
    Conf.bar() shouldBe "42"
    Conf.baz() shouldBe "1 2"
    Conf.coo() shouldBe "1 5"
    Conf.escapedSingleQuote() shouldBe "A'B"
  }

  def assertTok(input: String, output: Seq[String]): Unit = {
    ArgumentTokenizer.tokenize(input) match {
      case Matched(output, remainingInput) if remainingInput.length == 0 =>
        // success
      case other =>
        throw new AssertionError(other.toString + " was not equal to " + Matched(output, new StringView("", 0)).toString)
    }
  }

  test ("correct tokenizations") {
    assertTok("", Nil)
    assertTok(""" """, Nil)
    assertTok("""a""", Seq("a"))
    assertTok("""\'""", Seq("'"))
    assertTok(""" \'""", Seq("'"))
    assertTok("""\' """, Seq("'"))
    assertTok(""" \' """, Seq("'"))
    assertTok("""  \'  """, Seq("'"))
    assertTok("a\nc", Seq("a", "c"))
    assertTok("a b\nc", Seq("a", "b", "c"))
    assertTok("--opt arg", Seq("--opt", "arg"))
    assertTok("--opt arg1 arg2", Seq("--opt", "arg1", "arg2"))
    assertTok("""\"""", Seq("\""))
    assertTok("''", Seq(""))
    assertTok("''''", Seq(""))
    assertTok("""""""", Seq(""))
    assertTok("""""""""", Seq(""))
    assertTok("\\a", Seq("a"))
    assertTok("\\'", Seq("'"))
    assertTok("\\\"", Seq("\""))
    assertTok("a\tb", Seq("a", "b"))
    assertTok("'A B C'", Seq("A B C"))
    assertTok("'A B C' 'C D' ", Seq("A B C", "C D"))
    assertTok("--opt='A B C'", Seq("--opt=A B C"))
    assertTok(""" "a" """, Seq("a"))
    assertTok(""""a """", Seq("a "))
    assertTok(""" "a " """, Seq("a "))
    assertTok(""" " a" """, Seq(" a"))
    assertTok(""" " a " """, Seq(" a "))
    assertTok("--opt=\"A B C\" ", Seq("--opt=A B C"))
    assertTok("\"\\a\"", Seq("\\a"))
    assertTok("\"\\'\"", Seq("\\'"))
    assertTok(""" \" \\ \" """, Seq("\"", "\\", "\""))
    assertTok(""" \" \a \" """, Seq("\"", "a", "\""))
    assertTok(""" \" \\a \" """, Seq("\"", "\\a", "\""))
    assertTok(""" " \\ " """, Seq(" \\ "))
    assertTok(""" " \a " """, Seq(" \\a "))
    assertTok(""" " \\a " """, Seq(" \\a "))
    assertTok(""" " \\\" " """, Seq(""" \" """))
    assertTok(""" aoeu'X' """, Seq("aoeuX"))
    assertTok(""" aoeu"X"123 """, Seq("aoeuX123"))
    assertTok(""" aoeu"X"123'4' """, Seq("aoeuX1234"))
    assertTok(""" aoeu"X"123'4 5' """, Seq("aoeuX1234 5"))
    assertTok(""" \b """, Seq("b"))
    assertTok(""" "a" "b" """, Seq("a", "b"))
    assertTok(""" "a " " b" """, Seq("a ", " b"))
    assertTok(""" "a "" b" """, Seq("a  b"))
  }

  test ("eof") {
    ArgumentTokenizer.tokenize("--opt 'a") shouldBe EOF("'")
    ArgumentTokenizer.tokenize("--opt 'a\nb") shouldBe EOF("'")
    ArgumentTokenizer.tokenize("--opt 'a   b") shouldBe EOF("'")
    ArgumentTokenizer.tokenize("--opt \"a") shouldBe EOF("\"")
    ArgumentTokenizer.tokenize("--opt \"a\" \"b ") shouldBe EOF("\"")
    ArgumentTokenizer.tokenize("--opt \"a\" \"b \n") shouldBe EOF("\"")
    ArgumentTokenizer.tokenize("\\") shouldBe EOF("escaped char")
    ArgumentTokenizer.tokenize("--opt=\\") shouldBe EOF("escaped char")
  }

}
