package org.rogach.scrollop

trait ValueConverter[A] {
  def parse(s:List[List[String]]):Either[Unit,Option[A]]
}

