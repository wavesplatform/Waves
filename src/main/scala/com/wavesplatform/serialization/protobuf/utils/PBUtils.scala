package com.wavesplatform.serialization.protobuf.utils
import com.google.protobuf.CodedOutputStream
import scalapb.GeneratedMessage

object PBUtils {
  def encodeDeterministic(msg: GeneratedMessage): Array[Byte] = {
    val a            = new Array[Byte](msg.serializedSize)
    val outputStream = CodedOutputStream.newInstance(a)
    outputStream.useDeterministicSerialization() // Adds this
    msg.writeTo(outputStream)
    outputStream.checkNoSpaceLeft()
    a
  }
}
