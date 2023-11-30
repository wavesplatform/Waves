package com.wavesplatform.block.serialization

import java.nio.ByteBuffer
import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.{DigestLength, SignatureLength}
import com.wavesplatform.serialization.ByteBufferOps

import scala.util.Try

object MicroBlockSerializer {
  def toBytes(microBlock: MicroBlock): Array[Byte] = {
    val transactionDataBytes = writeTransactionData(microBlock.version, microBlock.transactionData)
    Bytes.concat(
      Array(microBlock.version),
      microBlock.reference.arr,
      microBlock.totalResBlockSig.arr,
      Ints.toByteArray(transactionDataBytes.length),
      transactionDataBytes,
      microBlock.sender.arr,
      microBlock.signature.arr,
      microBlock.stateHash.map(_.arr).getOrElse(Array.emptyByteArray)
    )
  }

  def parseBytes(bytes: Array[Byte]): Try[MicroBlock] =
    Try {
      val buf = ByteBuffer.wrap(bytes).asReadOnlyBuffer()

      val version          = buf.get
      val reference        = ByteStr(buf.getByteArray(Block.referenceLength(version)))
      val totalResBlockSig = ByteStr(buf.getByteArray(SignatureLength))

      buf.getInt

      val transactionData = readTransactionData(version, buf)
      val generator       = buf.getPublicKey
      val signature       = ByteStr(buf.getByteArray(SignatureLength))
      val stateHash       = buf.getByteArrayOpt(DigestLength).map(ByteStr(_))

      MicroBlock(version, generator, transactionData, reference, totalResBlockSig, signature, stateHash)
    }
}
