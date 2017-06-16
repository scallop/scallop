package org.rogach.scallop

trait ScallopArgListLoader {
  def loadArgList(args: Seq[String]): Seq[String] =
    if (args.headOption map("@--" ==) getOrElse false) {
      // read options from stdin
      io.Source.fromInputStream(java.lang.System.in).getLines.toList
      .flatMap(_.split(" ").filter(_.size > 0))
    } else if (args.headOption map(_ startsWith "@") getOrElse false) {
      // read options from a file (canned config)
      io.Source.fromFile(args.head.drop(1)).getLines.toList
      .flatMap(_.split(" ").filter(_.size > 0))
    } else {
      args
    }
}
