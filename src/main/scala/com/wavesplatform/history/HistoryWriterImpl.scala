package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.state2.{BlockDiff, ByteStr, DataTypes}
import com.wavesplatform.utils._
import kamon.Kamon
import scorex.block.Block
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{HistoryWriter, Transaction, ValidationError}
import scorex.utils.{LogMVMapBuilder, ScorexLogging}

import scala.util.Try

class HistoryWriterImpl private(file: Option[File], val synchronizationToken: ReentrantReadWriteLock) extends HistoryWriter {

  import HistoryWriterImpl._

  private val db = createMVStore(file)
  private val blockBodyByHeight = Synchronized(db.openMap("blocks", new LogMVMapBuilder[Int, Array[Byte]]))
  private val blockIdByHeight = Synchronized(db.openMap("signatures", new LogMVMapBuilder[Int, ByteStr].valueType(DataTypes.byteStr)))
  private val heightByBlockId = Synchronized(db.openMap("signaturesReverse", new LogMVMapBuilder[ByteStr, Int].keyType(DataTypes.byteStr)))
  private val scoreByHeight = Synchronized(db.openMap("score", new LogMVMapBuilder[Int, BigInt]))

  private val blockHeightStats = Kamon.metrics.histogram("block-height")

  private[HistoryWriterImpl] def isConsistent: Boolean = read { implicit l =>
    // check if all maps have same size
    Set(blockBodyByHeight().size(), blockIdByHeight().size(), heightByBlockId().size(), scoreByHeight().size()).size == 1
  }

  override def appendBlock(block: Block)(consensusValidation: => Either[ValidationError, BlockDiff]): Either[ValidationError, BlockDiff] = write { implicit lock =>
    if ((height() == 0) || (this.lastBlock.get.uniqueId == block.reference)) consensusValidation.map { blockDiff =>
      val h = height() + 1
      val score = (if (height() == 0) BigInt(0) else this.score()) + block.blockScore
      blockBodyByHeight.mutate(_.put(h, block.bytes))
      scoreByHeight.mutate(_.put(h, score))
      blockIdByHeight.mutate(_.put(h, block.uniqueId))
      heightByBlockId.mutate(_.put(block.uniqueId, h))

      db.commit()
      blockHeightStats.record(h)

      if (h % 100 == 0) db.compact(CompactFillRate, CompactMemorySize)

      blockDiff
    }
    else {
      Left(GenericError(s"Parent ${block.reference} of block ${block.uniqueId} does not match last local block ${this.lastBlock.map(_.uniqueId)}"))
    }
  }

  override def discardBlock(): Seq[Transaction] = write { implicit lock =>
    val h = height()
    val transactions =
      Block.parseBytes(blockBodyByHeight.mutate(_.remove(h))).fold(_ => Seq.empty[Transaction], _.transactionData)
    scoreByHeight.mutate(_.remove(h))
    val vOpt = Option(blockIdByHeight.mutate(_.remove(h)))
    vOpt.map(v => heightByBlockId.mutate(_.remove(v)))
    db.commit()

    transactions
  }


  override def lastBlockIds(howMany: Int): Seq[ByteStr] = read { implicit lock =>
    (Math.max(1, height() - howMany + 1) to height()).flatMap(i => Option(blockIdByHeight().get(i)))
      .reverse
  }

  override def height(): Int = read { implicit lock => blockIdByHeight().size() }

  override def scoreOf(id: ByteStr): Option[BlockchainScore] = read { implicit lock =>
    heightOf(id).map(scoreByHeight().get(_))
  }

  override def heightOf(blockSignature: ByteStr): Option[Int] = read { implicit lock =>
    Option(heightByBlockId().get(blockSignature))
  }

  override def blockBytes(height: Int): Option[Array[Byte]] = read { implicit lock =>
    Option(blockBodyByHeight().get(height))
  }

  override def close(): Unit = db.close()
}

object HistoryWriterImpl extends ScorexLogging {
  private val CompactFillRate = 90
  private val CompactMemorySize = 10 * 1024 * 1024

  def apply(file: Option[File], synchronizationToken: ReentrantReadWriteLock): Try[HistoryWriterImpl] =
    createWithStore[HistoryWriterImpl](file, new HistoryWriterImpl(file, synchronizationToken), h => h.isConsistent)
}
