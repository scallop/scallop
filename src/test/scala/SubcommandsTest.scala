package org.rogach.scallop

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class SubcommandsTest extends UsefulMatchers {
  throwError.value = true

  test ("builder") {
    val sub = Scallop()
      .opt[Boolean]("bananas")
    val opts = Scallop()
      .opt[Boolean]("apples")
      .addSubBuilder("tree",sub)
      .args(Seq("-a","tree","-b"))
      .verify
    opts.get[Boolean]("apples") should equal (Some(true))
    opts.get[Boolean]("tree\0bananas") should equal (Some(true))
    opts.getSubcommandName should equal (Some("tree"))
  }

  test ("conf") {
    object Conf extends ScallopConf(Seq("-a", "tree", "-b")) {
      val apples = opt[Boolean]("apples")
      val tree = new Subcommand("tree") {
        val bananas = opt[Boolean]("bananas")
      }
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
    }
    Conf.apples.isSupplied ==== false
    Conf.tree.bananas.isSupplied ==== false
  }

  test ("two nested configs") {
    object Conf extends ScallopConf(Seq("palm","-b")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Boolean]("apples")
      }
      val palm = new Subcommand("palm") {
        val bananas = opt[Boolean]("bananas")
      }
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
          }
        }
      }
    }
    Conf.subcommands ==== List(Conf.sub1, Conf.sub1.sub2, Conf.sub1.sub2.sub3, Conf.sub1.sub2.sub3.sub4)
    Conf.sub1.sub2.sub3.sub4.opts() ==== List("win!")
  }

  test ("subcommand, with some args pre-defined") {
    object Conf extends ScallopConf(Seq("tree", "-b")) {
      val tree = new ScallopConf(Seq("-a"), "tree") {
        val apples = opt[Boolean]("apples")
        val bananas = opt[Boolean]("bananas")
      }
    }
    Conf.tree.apples() ==== true
    Conf.tree.bananas() ==== true
  }

  test ("equal names in two subcommands") {
    object Conf extends ScallopConf(Seq("tree","-a")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Boolean]("apples")
      }
      val palm = new Subcommand("palm") {
        val apples = opt[Boolean]("apples")
      }
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
    }
    Conf.tree.apples() ==== true
    Conf.tree.bananas() ==== true
  }

  test ("codependency, inside subcommand, failure") {
    object Conf extends ScallopConf(Seq("tree", "-a")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Boolean]("apples")
        val bananas = opt[Boolean]("bananas")
        codependent(apples, bananas)
      }
    }
    intercept[OptionSetValidationFailure] {
      Conf
    }
  }

  test ("codependency, across confs, success") {
    object Conf extends ScallopConf(Seq("-a", "tree", "-b")) {
      val apples = opt[Boolean]("apples")
      val tree = new Subcommand("tree") {
        val bananas = opt[Boolean]("bananas")
      }
      codependent(apples, tree.bananas)
    }
    Conf.apples() ==== true
    Conf.tree.bananas() ==== true
  }

  test ("codependency, across confs, failure") {
    object Conf extends ScallopConf(Seq("-a", "tree")) {
      val apples = opt[Boolean]("apples")
      val tree = new Subcommand("tree") {
        val bananas = opt[Boolean]("bananas")
      }
      codependent(apples, tree.bananas)
    }
    intercept[OptionSetValidationFailure] {
      Conf
    }
  }

  test ("no-option subcommands") {
    object Conf extends ScallopConf(Seq("tree")) {
      val tree = new Subcommand("tree") {()}
    }
    Conf.subcommand ==== Some(Conf.tree)
  }

  test ("isSupplied, if there are no selected subcommands") {
    object Conf extends ScallopConf(Nil) {
      val tree = new Subcommand("tree") {
        val apples = opt[Int]("apples")
      }
    }
    Conf.tree.apples.isSupplied ==== false
  }

  test ("isSupplied, for other subcommand") {
    object Conf extends ScallopConf(Seq("tree", "-a", "42")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Int]("apples")
      }
      val palm = new Subcommand("palm") {
        val bananas = opt[Int]("bananas")
      }
    }
    Conf.palm.bananas.isSupplied ==== false
  }

  test ("subcommand name as parameter to other subcommand") {
    object Conf extends ScallopConf(Seq("help", "tree")) {
      val tree = new Subcommand("tree") {()}

      val help = new Subcommand("help")  {
        val command = trailArg[String]()
      }
    }
    Conf.help.command() ==== "tree"
  }

  test ("properties on subcommand") {
    object Conf extends ScallopConf(Seq("sub", "-Dkey1=value1")) {
      val sub = new Subcommand("sub") {
        val properties = props[String]('D')
      }
    }
    Conf.sub.properties ==== Map("key1" -> "value1")
  }

}
