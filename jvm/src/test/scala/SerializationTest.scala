package org.rogach.scallop

import java.io.{ByteArrayOutputStream, ByteArrayInputStream, ObjectOutputStream, ObjectInputStream}

class TestConf(args: List[String]) extends ScallopConf(args) with Serialization {
  val apples = opt[Int]("apples")
  verify()
}

class SerializationTest extends ScallopTestBase {

  test ("simple ScallopConf serialization") {
    val conf = new TestConf(List("-a","3"))

    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(conf)
    out.flush()
    out.close();

    val bytes = bos.toByteArray()

    val bis = new ByteArrayInputStream(bytes);
    val in = new ObjectInputStream(bis);

    val conf2 = in.readObject().asInstanceOf[TestConf]
    conf2.apples() shouldBe 3
  }

}
