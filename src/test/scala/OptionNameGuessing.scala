package org.rogach.scallop

import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class OptionNameGuessing extends UsefulMatchers {
  throwError.value = true

  test ("simple") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      val appleso = opt[Int]()
      val bananaso = opt[Int]()
    }
    Conf.appleso() should equal (1)
    Conf.bananaso.get should equal (None)
  }
  
  test ("tricky") {
    object Conf extends ScallopConf(Seq("-a", "1")) {
      val appleso = opt[Int]()
      val applesPlus = appleso.map(2+)
      lazy val applesVal = appleso()
      val bananaso = opt[Int]()
      val aaa = opt[Int]()
    }
    Conf.appleso() should equal (1)
    Conf.bananaso.get should equal (None)
    Conf.aaa.get should equal (None)
  }
  
  test ("camelCase convert") {
    object Conf extends ScallopConf(Seq("--apple-treeo", "1")) {
      val appleTreeo = opt[Int]()
    }
    Conf.appleTreeo() should equal (1)
  }
  
  test ("guessing in subcommands") {
    object Conf extends ScallopConf(Seq("tree", "--apples", "3")) {
      val tree = new Subcommand("tree") {
        val apples = opt[Int]()
      }
    }
    Conf.tree.apples() should equal (3)
  }
  
  test ("replacing of the name in mapped ScallopOptions") {
    object Conf extends ScallopConf(Nil) { 
      val xs = opt[Int](default = Some(0)) map (1+) 
    }
    Conf.xs.get ==== Some(1)
  }
  
  test ("replacing of the name in doubly mapped ScallopOptions") {
    object Conf extends ScallopConf(Nil) { 
      val xs = opt[Int](default = Some(0)) map (1+) map (_.toString)
    }
    Conf.xs.get ==== Some("1")
  }

}
