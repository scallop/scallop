package org.rogach.scallop

trait ScallopArgListLoader {
  def loadArgList(args: Seq[String]): Seq[String] = args
}
