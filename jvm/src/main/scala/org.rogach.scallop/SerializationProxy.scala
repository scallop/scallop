package org.rogach.scallop

import java.io.{Serializable, ObjectOutputStream, ObjectInputStream}

/** Use this trait to make your ScallopConf serializable.
  * Warning: this serialization method expects your ScallopConf class
  * to have public constructor that accepts Seq[String] or List[String] as a single parameter.
  *
  * If your class does not have such constructor, you will need to implement
  * serialization/deserialization logic yourself (see https://github.com/scallop/scallop/issues/137)
  */
trait Serialization extends Serializable { this: ScallopConf =>

  protected final def writeReplace(): AnyRef =
    new SerializationProxy(this, this.getClass.getName)

}

class SerializationProxy(
  @transient private var orig: ScallopConf,
  val confClassName: String
) extends Serializable {

  private def writeObject(out: ObjectOutputStream) {
    out.defaultWriteObject()
    out.writeObject(orig.args.toArray)
  }
  private def readObject(in: ObjectInputStream) {
    in.defaultReadObject()
    val args = in.readObject().asInstanceOf[Array[String]].toList

    val confClass = java.lang.Class.forName(confClassName)

    try {
      val seqConstructor = confClass.getConstructor(classOf[scala.collection.Seq[_]])
      orig = seqConstructor.newInstance(args).asInstanceOf[ScallopConf]
      return
    } catch {
      case _: Throwable => {}
    }

    try {
      val listConstructor = confClass.getConstructor(classOf[scala.collection.immutable.List[_]])
      orig = listConstructor.newInstance(args).asInstanceOf[ScallopConf]
      return
    } catch {
      case _: Throwable => {}
    }
  }

  private def readResolve(): AnyRef = orig

}
