package org.rogach.scallop

import scala.collection.{Seq => CSeq}

trait ScallopArgListLoader {
  def loadArgList(args: CSeq[String]): CSeq[String] = args
}
