package com.wavesplatform.protobuf.utils
import com.google.protobuf.{ByteString, CodedOutputStream}
import scalapb.GeneratedMessage

object PBUtils {
  def encodeDeterministic(msg: GeneratedMessage): Array[Byte] = {
    val outArray     = new Array[Byte](msg.serializedSize)
    val outputStream = CodedOutputStream.newInstance(outArray)
    outputStream.useDeterministicSerialization() // Adds this
    msg.writeTo(outputStream)
    outputStream.checkNoSpaceLeft()
    outArray
  }

  def toByteStringUnsafe(bs: Array[Byte]): ByteString = {
    val cls = Class.forName("com.google.protobuf.LiteralByteString")
    val cons = cls.getDeclaredConstructor(classOf[Array[Byte]])
    cons.setAccessible(true)
    cons.newInstance(bs).asInstanceOf[ByteString]
  }

  def toByteArrayUnsafe(bs: ByteString): Array[Byte] = {
    val cls = Class.forName("com.google.protobuf.LiteralByteString")
    if (cls.isInstance(bs)) {
      val m = cls.getDeclaredField("bytes")
      m.setAccessible(true)
      m.get(bs).asInstanceOf[Array[Byte]]
    } else {
      bs.toByteArray
    }
  }
}
