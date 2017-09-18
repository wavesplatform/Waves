package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.FeatureStatus
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
  private val featuresAtHeight = Synchronized(db.openMap("features-at-height", new LogMVMapBuilder[Int, Set[Short]].valueType(DataTypes.featureIds)))
  private val featuresState = Synchronized(db.openMap("features-state", new LogMVMapBuilder[Int, Map[Short, Byte]].valueType(DataTypes.featureState)))

  private val blockHeightStats = Kamon.metrics.histogram("block-height")

  private[HistoryWriterImpl] def isConsistent: Boolean = read { implicit l =>
    // check if all maps have same size
    Set(blockBodyByHeight().size(), blockIdByHeight().size(), heightByBlockId().size(), scoreByHeight().size()).size == 1
  }

  private def updateFeaturesState(height: Int): Unit = write { implicit lock =>
    require(height % FeatureApprovalBlocksCount == 0, s"Features' state can't be updated at given height: $height")

    val activated = (Math.max(1, height - FeatureApprovalBlocksCount + 1) to height)
      .flatMap(h => featuresAtHeight.mutate(_.get(h)))
      .foldLeft(Map.empty[Short, Int]) { (counts, feature) =>
        counts.updated(feature, counts.getOrElse(feature, 0) + 1)
      }.filter(p => p._2 >= MinBlocksCountToActivateFeature)
      .keys.map(k => k -> FeatureStatus.Accepted.status).toMap

    val previousApprovalHeight = Math.max(FeatureApprovalBlocksCount, height - FeatureApprovalBlocksCount)

    val previousState: Map[Short, Byte] = Option(featuresState().get(previousApprovalHeight))
      .getOrElse(Map.empty[Short, Byte])
      .mapValues(v => if (v == FeatureStatus.Accepted.status) FeatureStatus.Activated.status else v)

    val combined: Map[Short, Byte] = previousState ++ activated
    featuresState.mutate(_.put(height, combined))
  }

  override def appendBlock(block: Block)(consensusValidation: => Either[ValidationError, BlockDiff]): Either[ValidationError, BlockDiff] = write { implicit lock =>
    if ((height() == 0) || (this.lastBlock.get.uniqueId == block.reference)) consensusValidation.map { blockDiff =>
      val h = height() + 1
      val score = (if (height() == 0) BigInt(0) else this.score()) + block.blockScore
      blockBodyByHeight.mutate(_.put(h, block.bytes))
      scoreByHeight.mutate(_.put(h, score))
      blockIdByHeight.mutate(_.put(h, block.uniqueId))
      heightByBlockId.mutate(_.put(block.uniqueId, h))
      featuresAtHeight.mutate(_.put(h, block.supportedFeaturesIds))

      if (h % FeatureApprovalBlocksCount == 0) updateFeaturesState(h)

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

    featuresAtHeight.mutate(_.remove(h))
    if (h % FeatureApprovalBlocksCount == 0) {
      featuresState.mutate(_.remove(h))
    }

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

  override def status(feature: Short): FeatureStatus = read { implicit lock =>
    val h = height()
    val lastApprovalHeight = h - h % FeatureApprovalBlocksCount
    Option(featuresState().get(lastApprovalHeight)).map { m =>
      val byte: Byte = m.getOrElse(feature, FeatureStatus.Defined.status)
      FeatureStatus(byte)
    }.getOrElse(FeatureStatus.Defined)
  }
}

object HistoryWriterImpl extends ScorexLogging {
  private val CompactFillRate = 90
  private val CompactMemorySize = 10 * 1024 * 1024
  private val FeatureApprovalBlocksCount = 10000
  private val MinBlocksCountToActivateFeature = 9000

  def apply(file: Option[File], synchronizationToken: ReentrantReadWriteLock): Try[HistoryWriterImpl] =
    createWithStore[HistoryWriterImpl](file, new HistoryWriterImpl(file, synchronizationToken), h => h.isConsistent)
}
