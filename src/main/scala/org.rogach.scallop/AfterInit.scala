package org.rogach.scallop

/** Magic trait, that makes it possible to run code from superclass
  * after all subclasses initialization.
  */
trait AfterInit extends DelayedInit {
  def afterInit()
  private var initCount = 0
  private def getInitNumber(clazz: Class[_]):Int =
    if (clazz.getSuperclass == classOf[java.lang.Object]) 0 else getInitNumber(clazz.getSuperclass) + 1
  final def delayedInit(x: => Unit) {
    x
    initCount += 1
    if (getInitNumber(this.getClass) + 1 == initCount) afterInit
  }
}
