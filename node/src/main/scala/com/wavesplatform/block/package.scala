package com.wavesplatform

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.block.Block.{GenesisBlockVersion, NgBlockVersion, PlainBlockVersion, RewardBlockVersion}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.serialization.Deser.ByteBufferOps
import com.wavesplatform.transaction.{Transaction, TransactionParsers, TxVersion}

import scala.util.Try

package object block {
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

  private[block] implicit class ValidationOps[A](v: Either[ValidationError, A]) {
    def asTry: Try[A] = v.left.map(ve => new IllegalArgumentException(ve.toString)).toTry
  }
}
