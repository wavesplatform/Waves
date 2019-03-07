package com.wavesplatform.protobuf.utils
import com.google.protobuf.CodedOutputStream
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
}
