package com.wavesplatform.protobuf.utils
import com.google.protobuf.{ByteString, CodedOutputStream}
import scalapb.GeneratedMessage

import scala.util.{Failure, Success, Try}

//noinspection ScalaStyle
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
    Try(Class.forName("com.google.protobuf.ByteString")) match {
      case Success(cls) =>
        val cons = cls.getDeclaredMethod("wrap", classOf[Array[Byte]])
        cons.setAccessible(true)
        cons.invoke(null, bs).asInstanceOf[ByteString]

      case Failure(_) =>
        ???
        ByteString.copyFrom(bs)
    }
  }

  def toByteArrayUnsafe(bs: ByteString): Array[Byte] = {
    Try(bs.getClass.getDeclaredField("bytes")) match {
      case Success(m) =>
        m.setAccessible(true)
        m.get(bs).asInstanceOf[Array[Byte]]

      case Failure(_) =>
        ???
        bs.toByteArray
    }
  }
}
