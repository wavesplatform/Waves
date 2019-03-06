package com.wavesplatform.protobuf.transaction
import com.google.protobuf.CodedOutputStream
import com.wavesplatform.protobuf.utils.PBUtils

private[transaction] object PBTransactionSerialization {
  def signedBytes(tx: PBSignedTransaction): Array[Byte] = {
    val outArray     = new Array[Byte](tx.serializedSize + 2)
    val outputStream = CodedOutputStream.newInstance(outArray)
    outputStream.useDeterministicSerialization()

    outputStream.write(0xff.toByte)
    outputStream.write(0x01.toByte)
    tx.writeTo(outputStream)

    outputStream.flush()
    outputStream.checkNoSpaceLeft()

    outArray
  }

  def unsignedBytes(unsignedTx: PBTransaction): Array[Byte] = {
    PBUtils.encodeDeterministic(unsignedTx)
  }
}
