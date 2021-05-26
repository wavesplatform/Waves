package com.wavesplatform.block.serialization

import java.io.{ByteArrayInputStream, DataInputStream}

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.serialization.DataInputStreamOps

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
      microBlock.signature.arr
    )
  }

  def parseBytes(bytes: Array[Byte]): Try[MicroBlock] =
    Try {
      val di = new DataInputStream(new ByteArrayInputStream(bytes))

      val version          = di.readByte()
      val reference        = ByteStr(di.readByteArray(Block.referenceLength(version)))
      val totalResBlockSig = ByteStr(di.readByteArray(SignatureLength))

      di.readInt

      val transactionData = readTransactionData(version, di)
      val generator       = di.readPublicKey()
      val signature       = ByteStr(di.readByteArray(SignatureLength))

      MicroBlock(version, generator, transactionData, reference, totalResBlockSig, signature)
    }
}
