package com.wavesplatform.transaction.protobuf
import com.google.protobuf.CodedOutputStream

object PBTransactionSerialization {
  def bytesWithTypePrefix(tx: PBTransaction): Array[Byte] = {
    val outArray     = new Array[Byte](tx.serializedSize + 2)
    val outputStream = CodedOutputStream.newInstance(outArray)

    outputStream.write(PBTransaction.typeId)
    outputStream.write(PBTransaction.version)
    tx.writeTo(outputStream)

    outputStream.checkNoSpaceLeft()
    outArray
  }

  def unsignedBytes(tx: PBTransaction): Array[Byte] = {
    val unsignedTx   = tx.clearProofsArray
    val prefixLength = 3 // "WTX"
    val outArray     = new Array[Byte](unsignedTx.serializedSize + prefixLength)
    val outputStream = CodedOutputStream.newInstance(outArray)

    outputStream.useDeterministicSerialization() // Do not remove
    outputStream.write('W'.toByte)
    outputStream.write('T'.toByte)
    outputStream.write(unsignedTx.chainId.toByte)

    unsignedTx.writeTo(outputStream)
    outputStream.flush()
    outputStream.checkNoSpaceLeft()

    outArray
  }
}
