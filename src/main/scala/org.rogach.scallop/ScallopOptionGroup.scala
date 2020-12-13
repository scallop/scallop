package org.rogach.scallop

import scala.collection.mutable

/** Group of options. Options added to the group will be shown before all other options in help output, in the same order as they were added in.
  * (options without groups will be sorted alphabetically and shown after all option groups)
  * @param header Header string that will be printed immediately before corresponding options.
  */
class ScallopOptionGroup(val header: String) {
  private[scallop] val options = new mutable.ArrayBuffer[CliOption]()

  /** Add options to the end of this option group */
  def append(opts: ScallopOptionBase*): Unit = {
    val descriptors = opts.flatMap(_.cliOption)
    // already added options will move to the end of the list
    options --= descriptors // I would like to use subtractAll and appendAll, but unfortunately they are not available before Scala 2.13
    options ++= descriptors
  }
}
