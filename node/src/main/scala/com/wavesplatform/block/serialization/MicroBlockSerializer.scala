package com.wavesplatform.block.serialization

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.block.MicroBlock
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.serialization.ByteBufferOps

import scala.util.Try

object MicroBlockSerializer {
  def toBytes(microBlock: MicroBlock): Array[Byte] = {
    val transactionDataBytes = writeTransactionData(microBlock.version, microBlock.transactionData)
    Bytes.concat(
      Array(microBlock.version),
      microBlock.prevResBlockSig.arr,
      microBlock.totalResBlockSig.arr,
      Ints.toByteArray(transactionDataBytes.length),
      transactionDataBytes,
      microBlock.sender.arr,
      microBlock.signature.arr
    )
  }

  def parseBytes(bytes: Array[Byte]): Try[MicroBlock] =
    Try {
      val buf = ByteBuffer.wrap(bytes).asReadOnlyBuffer()

      val version          = buf.get
      val prevResBlockSig  = ByteStr(buf.getByteArray(SignatureLength))
      val totalResBlockSig = ByteStr(buf.getByteArray(SignatureLength))

      buf.getInt

      val transactionData = readTransactionData(version, buf)
      val generator       = buf.getPublicKey
      val signature       = ByteStr(buf.getByteArray(SignatureLength))

      MicroBlock(version, generator, transactionData, prevResBlockSig, totalResBlockSig, signature)
    }
}
