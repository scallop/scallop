package org.rogach.scallop

import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import java.lang.System

trait CapturingTest {
  /** Captures all output from the *fn* block into two strings - (stdout, stderr). */
  def captureOutput(fn: => Unit): (String, String) = {
    val streamOut = new ByteArrayOutputStream()
    val streamErr = new ByteArrayOutputStream()
    Console.withOut(streamOut) {
      Console.withErr(streamErr) {
        fn
      }
    }
    (streamOut.toString, streamErr.toString)
  }

  /** Supresses exit in *fn* block. Returns list of exit statuses that were attempted. */
  def trapExit(fn: => Unit): List[Int] = {
    Compat.exitStatuses = List[Int]()
    Compat.trapExits = true
    try {
      throwError.withValue(false) {
        fn
      }
    } catch {
      case e:RuntimeException if e.getMessage == "trapped exit" =>
    }
    val exitStatuses = Compat.exitStatuses.reverse
    Compat.exitStatuses = List[Int]()
    exitStatuses
  }

  /** Supresses exits in *fn* block, and captures stdout/stderr. */
  def captureOutputAndExits(fn: => Unit): (String, String, List[Int]) = {
    var exits = List[Int]()
    val (out, err) = captureOutput {
      exits = trapExit(fn)
    }
    (out, err, exits)
  }

  /** Runs program with needed input. */
  def withInput[A](input:String)(fn: => A):A = {
    val normalIn = System.in
    val streamIn = new ByteArrayInputStream(input.getBytes)
    System.setIn(streamIn)
    val res = fn
    System.setIn(normalIn)
    res
  }

}
