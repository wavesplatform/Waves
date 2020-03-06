package com.wavesplatform.block.serialization

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.serialization.ByteBufferOps

import scala.util.Try

object MicroBlockSerializer {
  def toBytes(microBlock: MicroBlock): Array[Byte] = {
    val transactionDataBytes = writeTransactionData(microBlock.version, microBlock.transactionData)
    Bytes.concat(
      Array(microBlock.version),
      microBlock.prevResBlockRef,
      microBlock.totalResBlockRef,
      Ints.toByteArray(transactionDataBytes.length),
      transactionDataBytes,
      microBlock.sender,
      if (microBlock.version >= Block.ProtoBlockVersion) microBlock.totalSignature.ensuring(_.length == SignatureLength) else ByteStr.empty,
      microBlock.signature
    )
  }

  def parseBytes(bytes: Array[Byte]): Try[MicroBlock] =
    Try {
      val buf = ByteBuffer.wrap(bytes).asReadOnlyBuffer()

      val version          = buf.get
      val prevResBlockSig  = ByteStr(buf.getByteArray(Block.referenceLength(version)))
      val totalResBlockSig = ByteStr(buf.getByteArray(Block.referenceLength(version)))

      buf.getInt

      val transactionData = readTransactionData(version, buf)
      val generator       = buf.getPublicKey
      val totalSignature  = if (version >= Block.ProtoBlockVersion) ByteStr(buf.getByteArray(SignatureLength)) else ByteStr.empty
      val signature       = ByteStr(buf.getByteArray(SignatureLength))

      MicroBlock(version, generator, transactionData, prevResBlockSig, totalResBlockSig, signature, totalSignature)
    }
}
