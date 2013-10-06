package org.rogach.scallop

/** A class, that lazily encapsulates a map inside.
  */
class LazyMap[A,+B](under: => Map[A,B]) extends Map[A,B] {
  lazy val m = under
  def get(key: A) = m.get(key)
  def iterator = m.iterator
  def +[B1 >: B](kv: (A,B1)): Map[A, B1] = m + kv
  def -(key: A): Map[A,B] = m - key
}
