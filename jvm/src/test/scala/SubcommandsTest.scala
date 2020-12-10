package org.rogach.scallop

import org.rogach.scallop.exceptions._

class SubcommandsTest extends ScallopTestBase {

  test ("conf") {
    object Conf extends ScallopConf(Seq("-a", "tree", "-b")) {
      val apples = opt[Boolean]("apples")
      object tree extends Subcommand("tree") {
        val bananas = opt[Boolean]("bananas")
      }
      addSubcommand(tree)

      verify()
    }
    Conf.apples() should equal (true)
    Conf.apples.isSupplied shouldBe true
    Conf.subcommand shouldBe Some(Conf.tree)
    Conf.subcommands shouldBe List(Conf.tree)
    Conf.tree.bananas() should equal (true)
    Conf.tree.bananas.isSupplied shouldBe true
  }

  test ("options are not supplied") {
    object Conf extends ScallopConf(Seq()) {
      val apples = opt[Boolean]("apples")
      object tree extends Subcommand("tree") {
        val bananas = opt[Boolean]("bananas")
      }
      addSubcommand(tree)

      verify()
    }
    Conf.apples.isSupplied shouldBe false
    Conf.tree.bananas.isSupplied shouldBe false
  }

  test ("two nested configs") {
    object Conf extends ScallopConf(Seq("palm","-b")) {
      object tree extends Subcommand("tree") {
        val apples = opt[Boolean]("apples")
      }
      addSubcommand(tree)

      object palm extends Subcommand("palm") {
        val bananas = opt[Boolean]("bananas")
      }
      addSubcommand(palm)

      verify()
    }
    Conf.subcommand shouldBe Some(Conf.palm)
    Conf.palm.bananas() shouldBe true
    Conf.palm.bananas.isSupplied shouldBe true
    // pattern matching on subcommands
    (Conf.subcommand match {
      case Some(Conf.tree) => false
      case Some(Conf.palm) => true
      case _ => false
    }) shouldBe true
  }

  test ("4-level nested subcommands") {
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

  test ("equal names in two subcommands") {
    object Conf extends ScallopConf(Seq("tree","-a")) {
      object tree extends Subcommand("tree") {
        val apples = opt[Boolean]("apples")
      }
      addSubcommand(tree)

      val palm = new Subcommand("palm") {
        val apples = opt[Boolean]("apples")
      }
      addSubcommand(palm)

      verify()
    }
    Conf.tree.apples() shouldBe true
  }

  test ("codependency, inside subcommand, success") {
    object Conf extends ScallopConf(Seq("tree", "-a", "-b")) {
      object tree extends Subcommand("tree") {
        val apples = opt[Boolean]("apples")
        val bananas = opt[Boolean]("bananas")
        codependent(apples, bananas)
      }
      addSubcommand(tree)

      verify()
    }
    Conf.tree.apples() shouldBe true
    Conf.tree.bananas() shouldBe true
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
      object tree extends Subcommand("tree") {
        val bananas = opt[Boolean]("bananas")
      }
      addSubcommand(tree)

      codependent(apples, tree.bananas)

      verify()
    }
    Conf.apples() shouldBe true
    Conf.tree.bananas() shouldBe true
  }

  test ("codependency, across confs, failure") {
    expectException(ValidationFailure("Either all or none of the following options should be supplied, because they are co-dependent: apples, bananas")) {
      new ScallopConf(Seq("-a", "tree")) {
        val apples = opt[Boolean]("apples")
        object tree extends Subcommand("tree") {
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
      val tree = new Subcommand("tree") {}
      addSubcommand(tree)

      verify()
    }
    Conf.subcommand shouldBe Some(Conf.tree)
  }

  test ("isSupplied, if there are no selected subcommands") {
    object Conf extends ScallopConf(Nil) {
      object tree extends Subcommand("tree") {
        val apples = opt[Int]("apples")
      }
      addSubcommand(tree)

      verify()
    }
    Conf.tree.apples.isSupplied shouldBe false
  }

  test ("isSupplied, for other subcommand") {
    object Conf extends ScallopConf(Seq("tree", "-a", "42")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Int]("apples")
      }
      addSubcommand(tree)

      object palm extends Subcommand("palm") {
        val bananas = opt[Int]("bananas")
      }
      addSubcommand(palm)

      verify()
    }
    Conf.palm.bananas.isSupplied shouldBe false
  }

  test ("subcommand name as parameter to other subcommand") {
    object Conf extends ScallopConf(Seq("help", "tree")) {
      val tree = new Subcommand("tree") {}
      addSubcommand(tree)

      object help extends Subcommand("help")  {
        val command = trailArg[String]()
      }
      addSubcommand(help)

      verify()
    }
    Conf.help.command() shouldBe "tree"
  }

  test ("properties on subcommand") {
    object Conf extends ScallopConf(Seq("sub", "-Dkey1=value1")) {
      object sub extends Subcommand("sub") {
        val properties = props[String]('D')
      }
      addSubcommand(sub)

      verify()
    }
    Conf.sub.properties shouldBe Map("key1" -> "value1")
  }

  test ("subcommand name guessing") {
    object Conf extends ScallopConf(Seq("tree", "--apples", "42")) {
      object tree extends Subcommand("tree") {
        val apples = opt[Int]()
      }
      addSubcommand(tree)

      verify()
    }
    Conf.tree.apples.toOption shouldBe Some(42)
  }

  test ("isSupplied on subcommand options - true") {
    object Conf extends ScallopConf(Seq("tree", "--apples")) {
      object tree extends Subcommand("tree") {
        val apples = opt[Boolean]()
      }
      addSubcommand(tree)

      verify()
    }

    Conf.tree.apples.isSupplied shouldBe true
  }

  test ("isSupplied on subcommand options - false") {
    object Conf extends ScallopConf(Seq("tree")) {
      object tree extends Subcommand("tree") {
        val apples = opt[Boolean]()
      }
      addSubcommand(tree)

      verify()
    }

    Conf.tree.apples.isSupplied shouldBe false
  }

  test ("subcommand aliases") {
    object Conf extends ScallopConf(Seq("apple", "-c", "42")) {
      object fruit extends Subcommand("fruit", "apple", "banana") {
        val count = opt[Int]()
      }
      addSubcommand(fruit)

      verify()
    }
    Conf.subcommand shouldBe Some(Conf.fruit)
    Conf.subcommands shouldBe List(Conf.fruit)
    Conf.fruit.count() shouldBe 42
    Conf.fruit.count.isSupplied shouldBe true
  }

  test ("require subcommand to be present - successfull case") {
    object Conf extends ScallopConf(Seq("fruit", "-c", "42")) {
      object fruit extends Subcommand("fruit") {
        val count = opt[Int]()
      }
      addSubcommand(fruit)
      requireSubcommand()

      verify()
    }
    Conf.subcommand shouldBe Some(Conf.fruit)
    Conf.fruit.count() shouldBe 42
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
      object fruit extends Subcommand("fruit") {
        object pick extends Subcommand("pick") {
          val count = opt[Int]()
        }
        addSubcommand(pick)
        requireSubcommand()
      }
      addSubcommand(fruit)
      requireSubcommand()

      verify()
    }
    Conf.subcommand shouldBe Some(Conf.fruit)
    Conf.subcommands shouldBe List(Conf.fruit, Conf.fruit.pick)
    Conf.fruit.pick.count() shouldBe 42
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

  test ("sharing arguments with inheritance") {
    class Conf(args: Seq[String]) extends ScallopConf(args) {
      trait B { this: ScallopConf => // <<< NB: otherwise you will get Option identifier 'o' is not unique (calls `opt` on the parent class `Conf` twice)
        val o = opt[Boolean](name = "o")
      }
      object c1 extends Subcommand("c1") with B
      addSubcommand(c1)
      object c2 extends Subcommand("c2") with B
      addSubcommand(c2)
      verify()
    }

    new Conf(Seq("c1", "-o")).c1.o() shouldBe true
    new Conf(Seq("c2", "-o")).c2.o() shouldBe true
  }

  test ("verification on subcommands") {
    expectException(WrongOptionFormat("apples", "b", "bad Int value")) {
      object Conf extends ScallopConf(Seq("tree", "-a", "b")) {
        object tree extends Subcommand("tree") {
          val apples = opt[Int]()
        }
        addSubcommand(tree)

        verify()
      }
      Conf
    }
  }

  test ("validation failure on subcommands") {
    expectException(ValidationFailure("tree: a + b must be < 3")) {
      object Conf extends ScallopConf(Seq("tree", "-a", "1", "-b", "5")) {
        object tree extends Subcommand("tree") {
          val apples = opt[Int]()
          val bananas = opt[Int]()
          validate(apples, bananas) { (a, b) =>
            if (a + b >= 3) Left("tree: a + b must be < 3")
            else Right(())
          }
        }
        addSubcommand(tree)

        verify()
      }
      Conf
    }
  }

  test ("validation failure on nested subcommands") {
    expectException(ValidationFailure("branch: a + b must be < 3")) {
      object Conf extends ScallopConf(Seq("tree", "branch", "-a", "1", "-b", "5")) {
        object tree extends Subcommand("tree") {
          val branch = new Subcommand("branch") {
            val apples = opt[Int]()
            val bananas = opt[Int]()
            validate(apples, bananas) { (a, b) =>
              if (a + b >= 3) Left("branch: a + b must be < 3")
              else Right(())
            }
          }
          addSubcommand(branch)
        }
        addSubcommand(tree)

        verify()
      }
      Conf
    }
  }

  test ("validationOpt failure on subconfigs") {
    expectException(ValidationFailure("both a and b must be supplied")) {
      object Conf extends ScallopConf(Seq("tree", "-a", "1")) {
        object tree extends Subcommand("tree") {
          val apples = opt[Int]()
          val bananas = opt[Int]()
          validateOpt(apples, bananas) {
            case (Some(a), Some(b)) => Right(())
            case _ => Left("both a and b must be supplied")
          }
        }
        addSubcommand(tree)

        verify()
      }
      Conf
    }
  }


}
