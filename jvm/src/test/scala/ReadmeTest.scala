package org.rogach.scallop

/* Tests for examples in README.md */
class ReadmeTest extends ScallopTestBase {

  test ("quick example") {
    class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
      val apples = opt[Int](required = true)
      val bananas = opt[Int]()
      val name = trailArg[String]()
      verify()
    }

    val conf1 = new Conf(List("--apples", "4", "--bananas", "10", "strangeTree"))
    conf1.apples() shouldBe 4
    conf1.bananas() shouldBe 10
    conf1.name() shouldBe "strangeTree"

    val conf2 = new Conf(List("-a", "4", "appleTree"))
    conf2.apples() shouldBe 4
    conf2.name() shouldBe "appleTree"
  }

  test ("fancy things - matching on trailing arguments") {
    object Conf extends ScallopConf(
           List("-Ekey1=value1", "key2=value2", "key3=value3",
                "first", "1","2","3","second","4","5","6")) {
      val properties = props[String]('E')
      val firstListName = trailArg[String]()
      val firstList = trailArg[List[Int]]()
      val secondListName = trailArg[String]()
      val secondList = trailArg[List[Double]]()
      verify()
    }
    Conf.properties("key1") shouldBe "value1"
    Conf.firstListName() shouldBe "first"
    Conf.secondListName() shouldBe "second"
    Conf.firstList() shouldBe List(1,2,3)
    Conf.secondList() shouldBe List[Double](4,5,6)
  }

  test ("fancy things - nested subcommands") {
    object Conf extends ScallopConf(Seq("sub1", "sub2", "sub3", "sub4", "win!")) {
      object sub1 extends Subcommand("sub1") {
        object sub2 extends Subcommand("sub2") {
          object sub3 extends Subcommand("sub3") {
            object sub4 extends Subcommand("sub4") {
              val opts = trailArg[List[String]]()
            }
            addSubcommand(sub4)
          }
          addSubcommand(sub3)
        }
        addSubcommand(sub2)
      }
      addSubcommand(sub1)
      verify()
    }
    Conf.subcommands shouldBe List(Conf.sub1, Conf.sub1.sub2, Conf.sub1.sub2.sub3, Conf.sub1.sub2.sub3.sub4)
    Conf.sub1.sub2.sub3.sub4.opts() shouldBe List("win!")
  }

}
