package org.rogach.scallop

trait ValueConverter[A] {
  def parse(s:List[List[String]]):Either[Unit,Option[A]]
}

