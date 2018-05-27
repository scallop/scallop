package org.rogach.scallop

/** A class, that lazily encapsulates a map inside.
  */
class LazyMap[A,+B](under: => Map[A,B]) extends Map[A,B] {
  private[this] lazy val m = under
  def get(key: A) = m.get(key)
  def iterator = m.iterator
  def remove(key: A) = m.remove(key)
  def updated[B1 >: B](key: A, value: B1) = m.updated(key, value)
}
