package com.wavesplatform.block

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.block.Block.{GenesisBlockVersion, NgBlockVersion, PlainBlockVersion, RewardBlockVersion}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.ByteBufferOps
import com.wavesplatform.transaction.{Transaction, TransactionParsers, TxVersion}

package object serialization {
  private[block] def writeTransactionData(version: TxVersion, txs: Seq[Transaction]): Array[Byte] = {
    val txsCount = version match {
      case GenesisBlockVersion | PlainBlockVersion => Array(txs.size.toByte)
      case NgBlockVersion | RewardBlockVersion     => Ints.toByteArray(txs.size)
    }

    val txsBytesSize = txs.map(_.bytes().length + Ints.BYTES).sum
    val txsBuf       = ByteBuffer.allocate(txsBytesSize)
    txs.foreach(tx => txsBuf.put(Ints.toByteArray(tx.bytes().length)).put(tx.bytes()))

    Bytes.concat(txsCount, txsBuf.array())
  }

  private[block] def readTransactionData(version: Int, buf: ByteBuffer): Seq[Transaction] = {
    val txCount = version match {
      case Block.GenesisBlockVersion | Block.PlainBlockVersion => buf.get
      case Block.NgBlockVersion | Block.RewardBlockVersion     => buf.getInt
    }

    val txs = (1 to txCount).foldLeft(List.empty[Transaction]) {
      case (txs, _) =>
        val size = buf.getInt
        TransactionParsers.parseBytes(buf.getByteArray(size)).get :: txs
    }
    txs.reverse
  }

  private[block] def writeConsensusBytes(baseTarget: Long, generationSignature: ByteStr): Array[Byte] =
    Bytes.concat(
      Longs.toByteArray(baseTarget),
      generationSignature.arr
    )
}
