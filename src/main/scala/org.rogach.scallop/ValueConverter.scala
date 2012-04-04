package org.rogach.scallop

import scala.reflect.Manifest

trait ValueConverter[A] { parent =>
  def parse(s:List[List[String]]):Either[Unit,Option[A]]
  val manifest:Manifest[A]
  val argType:ArgType.V 
  
  def map[B](fn: A => B)(implicit m:Manifest[B]):ValueConverter[B] = new ValueConverter[B] { child =>
    def parse(s:List[List[String]]):Either[Unit,Option[B]] = parent.parse(s).right.map(_.map(fn))
    val manifest = m
    val argType = parent.argType
  }
  
}

