package org.rogach.scallop

trait ScallopOptionBase {
  private[scallop] val cliOption: Option[CliOption]
  def name: String
  def isSupplied: Boolean
}
