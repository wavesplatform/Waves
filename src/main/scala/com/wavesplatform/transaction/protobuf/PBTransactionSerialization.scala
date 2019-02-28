package com.wavesplatform.transaction.protobuf
import com.google.protobuf.CodedOutputStream
import com.wavesplatform.serialization.protobuf.utils.PBUtils

private[transaction] object PBTransactionSerialization {
  def signedBytes(tx: PBSignedTransaction): Array[Byte] = {
    val outArray     = new Array[Byte](tx.serializedSize + 2)
    val outputStream = CodedOutputStream.newInstance(outArray)
    outputStream.useDeterministicSerialization()

    outputStream.write(PBTransaction.typeId)
    outputStream.write(PBTransaction.version)
    tx.writeTo(outputStream)

    outputStream.flush()
    outputStream.checkNoSpaceLeft()

    outArray
  }

  def unsignedBytes(unsignedTx: PBTransaction): Array[Byte] = {
    PBUtils.encodeDeterministic(unsignedTx)
  }
}
