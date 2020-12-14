package org.rogach.scallop.tokenize

/** Wrapper class that allows us to have constant-time .substring without copying
  * (shares the underlying String between instances).
  *
  * This is needed because since Java 7 String.substring does a full copy instead of sharing the bytes.
  */
private[scallop] class StringView(underlying: String, offset: Int) {

  def substring(beginIndex: Int): StringView = {
    new StringView(underlying, offset + beginIndex)
  }

  def extract(beginIndex: Int, endIndex: Int): String = {
    underlying.substring(offset + beginIndex, offset + endIndex)
  }

  def length: Int = underlying.length - offset

  def charAt(index: Int): Char = underlying.charAt(offset + index)

  def indexOf(ch: Int, fromIndex: Int): Int = {
    val index = underlying.indexOf(ch, offset + fromIndex)
    if (index == -1) index
    else index - offset
  }

  override def toString(): String = {
    val b = new java.lang.StringBuilder()
    b.append("StringView(")
    b.append(underlying: CharSequence, offset: Int, underlying.length: Int)
    b.append(")")
    b.toString
  }

}
