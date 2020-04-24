package org.rogach.scallop

import org.scalatest.funsuite.AnyFunSuite
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class SubcommandsTest extends UsefulMatchers {
  throwError.value = true

  test ("builder") {
    val sub = Scallop()
      .opt[Boolean]("bananas")
    val opts = Scallop()
      .opt[Boolean]("apples")
      .addSubBuilder(Seq("tree"),sub)
      .args(Seq("-a","tree","-b"))
      .verify
    opts.get("apples") should equal (Some(true))
    opts.get("tree\u0000bananas") should equal (Some(true))
    opts.getSubcommandName should equal (Some("tree"))
  }

  test ("conf") {
    object Conf extends ScallopConf(Seq("-a", "tree", "-b")) {
      val apples = opt[Boolean]("apples")
      val tree = new Subcommand("tree") {
        val bananas = opt[Boolean]("bananas")
      }
      addSubcommand(tree)

      verify()
    }
    Conf.apples() should equal (true)
    Conf.apples.isSupplied ==== true
    Conf.subcommand ==== Some(Conf.tree)
    Conf.subcommands ==== List(Conf.tree)
    Conf.tree.bananas() should equal (true)
    Conf.tree.bananas.isSupplied ==== true
  }

  test ("options are not supplied") {
    object Conf extends ScallopConf(Seq()) {
      val apples = opt[Boolean]("apples")
      val tree = new Subcommand("tree") {
        val bananas = opt[Boolean]("bananas")
      }
      addSubcommand(tree)

      verify()
    }
    Conf.apples.isSupplied ==== false
    Conf.tree.bananas.isSupplied ==== false
  }

  test ("two nested configs") {
    object Conf extends ScallopConf(Seq("palm","-b")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Boolean]("apples")
      }
      addSubcommand(tree)

      val palm = new Subcommand("palm") {
        val bananas = opt[Boolean]("bananas")
      }
      addSubcommand(palm)

      verify()
    }
    Conf.subcommand ==== Some(Conf.palm)
    Conf.palm.bananas() ==== true
    Conf.palm.bananas.isSupplied ==== true
    // pattern matching on subcommands
    (Conf.subcommand match {
      case Some(Conf.tree) => false
      case Some(Conf.palm) => true
      case _ => false
    }) ==== true
  }

  test ("4-level nested subcommands") {
    object Conf extends ScallopConf(Seq("sub1", "sub2", "sub3", "sub4", "win!")) {
      val sub1 = new Subcommand("sub1") {
        val sub2 = new Subcommand("sub2") {
          val sub3 = new Subcommand("sub3") {
            val sub4 = new Subcommand("sub4") {
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
    Conf.subcommands ==== List(Conf.sub1, Conf.sub1.sub2, Conf.sub1.sub2.sub3, Conf.sub1.sub2.sub3.sub4)
    Conf.sub1.sub2.sub3.sub4.opts() ==== List("win!")
  }

  test ("equal names in two subcommands") {
    object Conf extends ScallopConf(Seq("tree","-a")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Boolean]("apples")
      }
      addSubcommand(tree)

      val palm = new Subcommand("palm") {
        val apples = opt[Boolean]("apples")
      }
      addSubcommand(palm)

      verify()
    }
    Conf.tree.apples() ==== true
  }

  test ("codependency, inside subcommand, success") {
    object Conf extends ScallopConf(Seq("tree", "-a", "-b")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Boolean]("apples")
        val bananas = opt[Boolean]("bananas")
        codependent(apples, bananas)
      }
      addSubcommand(tree)

      verify()
    }
    Conf.tree.apples() ==== true
    Conf.tree.bananas() ==== true
  }

  test ("codependency, inside subcommand, failure") {
    expectException(ValidationFailure("Either all or none of the following options should be supplied, because they are co-dependent: apples, bananas")) {
      new ScallopConf(Seq("tree", "-a")) {
        val tree = new Subcommand("tree") {
          val apples = opt[Boolean]("apples")
          val bananas = opt[Boolean]("bananas")
          codependent(apples, bananas)
        }
        addSubcommand(tree)

        verify()
      }
    }
  }

  test ("codependency, across confs, success") {
    object Conf extends ScallopConf(Seq("-a", "tree", "-b")) {
      val apples = opt[Boolean]("apples")
      val tree = new Subcommand("tree") {
        val bananas = opt[Boolean]("bananas")
      }
      addSubcommand(tree)

      codependent(apples, tree.bananas)

      verify()
    }
    Conf.apples() ==== true
    Conf.tree.bananas() ==== true
  }

  test ("codependency, across confs, failure") {
    expectException(ValidationFailure("Either all or none of the following options should be supplied, because they are co-dependent: apples, bananas")) {
      new ScallopConf(Seq("-a", "tree")) {
        val apples = opt[Boolean]("apples")
        val tree = new Subcommand("tree") {
          val bananas = opt[Boolean]("bananas")
        }
        addSubcommand(tree)

        codependent(apples, tree.bananas)

        verify()
      }
    }
  }

  test ("no-option subcommands") {
    object Conf extends ScallopConf(Seq("tree")) {
      val tree = new Subcommand("tree") {()}
      addSubcommand(tree)

      verify()
    }
    Conf.subcommand ==== Some(Conf.tree)
  }

  test ("isSupplied, if there are no selected subcommands") {
    object Conf extends ScallopConf(Nil) {
      val tree = new Subcommand("tree") {
        val apples = opt[Int]("apples")
      }
      addSubcommand(tree)

      verify()
    }
    Conf.tree.apples.isSupplied ==== false
  }

  test ("isSupplied, for other subcommand") {
    object Conf extends ScallopConf(Seq("tree", "-a", "42")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Int]("apples")
      }
      addSubcommand(tree)

      val palm = new Subcommand("palm") {
        val bananas = opt[Int]("bananas")
      }
      addSubcommand(palm)

      verify()
    }
    Conf.palm.bananas.isSupplied ==== false
  }

  test ("subcommand name as parameter to other subcommand") {
    object Conf extends ScallopConf(Seq("help", "tree")) {
      val tree = new Subcommand("tree") {()}
      addSubcommand(tree)

      val help = new Subcommand("help")  {
        val command = trailArg[String]()
      }
      addSubcommand(help)

      verify()
    }
    Conf.help.command() ==== "tree"
  }

  test ("properties on subcommand") {
    object Conf extends ScallopConf(Seq("sub", "-Dkey1=value1")) {
      val sub = new Subcommand("sub") {
        val properties = props[String]('D')
      }
      addSubcommand(sub)

      verify()
    }
    Conf.sub.properties ==== Map("key1" -> "value1")
  }

  test ("subcommand name guessing") {
    val conf = new ScallopConf(Seq("tree", "--apples", "42")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Int]()
      }
      addSubcommand(tree)

      verify()
    }
    conf.tree.apples.toOption shouldBe Some(42)
  }

  test ("isSupplied on subcommand options - true") {
    val conf = new ScallopConf(Seq("tree", "--apples")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Boolean]()
      }
      addSubcommand(tree)

      verify()
    }

    conf.tree.apples.isSupplied shouldBe true
  }

  test ("isSupplied on subcommand options - false") {
    val conf = new ScallopConf(Seq("tree")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Boolean]()
      }
      addSubcommand(tree)

      verify()
    }

    conf.tree.apples.isSupplied shouldBe false
  }

  test ("subcommand aliases") {
    object Conf extends ScallopConf(Seq("apple", "-c", "42")) {
      val fruit = new Subcommand("fruit", "apple", "banana") {
        val count = opt[Int]()
      }
      addSubcommand(fruit)

      verify()
    }
    Conf.subcommand ==== Some(Conf.fruit)
    Conf.subcommands ==== List(Conf.fruit)
    Conf.fruit.count() shouldEqual 42
    Conf.fruit.count.isSupplied ==== true
  }

  test ("require subcommand to be present - successfull case") {
    object Conf extends ScallopConf(Seq("fruit", "-c", "42")) {
      val fruit = new Subcommand("fruit") {
        val count = opt[Int]()
      }
      addSubcommand(fruit)
      requireSubcommand()

      verify()
    }
    Conf.subcommand ==== Some(Conf.fruit)
    Conf.fruit.count() shouldEqual 42
  }

  test ("require subcommand to be present - error case") {
    expectException(ValidationFailure("Subcommand required")) {
      object Conf extends ScallopConf(Seq()) {
        val fruit = new Subcommand("fruit") {
          val count = opt[Int]()
        }
        addSubcommand(fruit)
        requireSubcommand()

        verify()
      }
      Conf
    }
  }

  test ("require several nested subcommands to be present - successfull case") {
    object Conf extends ScallopConf(Seq("fruit", "pick", "-c", "42")) {
      val fruit = new Subcommand("fruit") {
        val pick = new Subcommand("pick") {
          val count = opt[Int]()
        }
        addSubcommand(pick)
        requireSubcommand()
      }
      addSubcommand(fruit)
      requireSubcommand()

      verify()
    }
    Conf.subcommand ==== Some(Conf.fruit)
    Conf.subcommands ==== List(Conf.fruit, Conf.fruit.pick)
    Conf.fruit.pick.count() shouldEqual 42
  }

  test ("require several nested subcommands to be present - error case") {
    expectException(ValidationFailure("Subcommand required")) {
      object Conf extends ScallopConf(Seq("fruit")) {
        val fruit = new Subcommand("fruit") {
          val pick = new Subcommand("pick") {
            val count = opt[Int]()
          }
          addSubcommand(pick)
          requireSubcommand()
        }
        addSubcommand(fruit)
        requireSubcommand()

        verify()
      }
      Conf
    }
  }

}
