package com.wavesplatform.block

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.block.Block.{GenesisBlockVersion, NgBlockVersion, PlainBlockVersion, ProtoBlockVersion, RewardBlockVersion}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.protobuf.transaction.{PBTransactions, SignedTransaction}
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.serialization.ByteBufferOps
import com.wavesplatform.transaction.{Transaction, TransactionParsers}

package object serialization {
  private[block] def writeTransactionData(version: Byte, txs: Seq[Transaction]): Array[Byte] = {
    val txsBytes     = txs.map(tx => if (version == ProtoBlockVersion) PBUtils.encodeDeterministic(PBTransactions.protobuf(tx)) else tx.bytes())
    val txsBytesSize = txsBytes.map(_.length + Ints.BYTES).sum
    val txsBuf       = ByteBuffer.allocate(txsBytesSize)
    txsBytes.foreach(tx => txsBuf.putInt(tx.length).put(tx))

    Bytes.concat(mkTxsCountBytes(version, txs.size), txsBuf.array())
  }

  private[block] def readTransactionData(version: Byte, buf: ByteBuffer): Seq[Transaction] = {
    val txCount = (version: @unchecked) match {
      case GenesisBlockVersion | PlainBlockVersion                 => buf.getByte
      case NgBlockVersion | RewardBlockVersion | ProtoBlockVersion => buf.getInt
    }

    val txs = (1 to txCount).foldLeft(List.empty[Transaction]) {
      case (txs, _) =>
        val size    = buf.getInt
        val txBytes = buf.getByteArray(size)
        val tx = version match {
          case ProtoBlockVersion => PBTransactions.vanilla(SignedTransaction.parseFrom(txBytes)).explicitGet()
          case _                 => TransactionParsers.parseBytes(txBytes).get
        }
        tx :: txs
    }
    txs.reverse
  }

  private[block] def writeConsensusBytes(baseTarget: Long, generationSignature: ByteStr): Array[Byte] =
    Bytes.concat(
      Longs.toByteArray(baseTarget),
      generationSignature.arr
    )

  def mkTxsCountBytes(version: Byte, txsCount: Int): Array[Byte] = (version: @unchecked) match {
    case GenesisBlockVersion | PlainBlockVersion                 => Array(txsCount.toByte)
    case NgBlockVersion | RewardBlockVersion | ProtoBlockVersion => Ints.toByteArray(txsCount)
  }
}
