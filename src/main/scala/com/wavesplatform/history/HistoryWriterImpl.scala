package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.{BlockchainFeatures, FeatureStatus}
import com.wavesplatform.settings.{FeaturesSettings, FunctionalitySettings}
import com.wavesplatform.state2.{BlockDiff, ByteStr, DataTypes}
import com.wavesplatform.utils._
import kamon.Kamon
import scorex.block.Block
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.utils.{LogMVMapBuilder, ScorexLogging}

import scala.util.Try

class HistoryWriterImpl private(file: Option[File], val synchronizationToken: ReentrantReadWriteLock,
                                functionalitySettings: FunctionalitySettings, featuresSettings: FeaturesSettings)
  extends History with ScorexLogging {

  import HistoryWriterImpl._

  private val FeatureApprovalBlocksCount = functionalitySettings.featureCheckBlocksPeriod
  private val MinBlocksCountToActivateFeature = functionalitySettings.blocksForFeatureActivation

  private val db = createMVStore(file)
  private val blockBodyByHeight = Synchronized(db.openMap("blocks", new LogMVMapBuilder[Int, Array[Byte]]))
  private val blockIdByHeight = Synchronized(db.openMap("signatures", new LogMVMapBuilder[Int, ByteStr].valueType(DataTypes.byteStr)))
  private val heightByBlockId = Synchronized(db.openMap("signaturesReverse", new LogMVMapBuilder[ByteStr, Int].keyType(DataTypes.byteStr)))
  private val scoreByHeight = Synchronized(db.openMap("score", new LogMVMapBuilder[Int, BigInt]))
  private val featuresAtHeight = Synchronized(db.openMap("features-at-height", new LogMVMapBuilder[Int, Set[Short]].valueType(DataTypes.featureIds)))
  private val featuresState = Synchronized(db.openMap("features-state", new LogMVMapBuilder[Int, Map[Short, Byte]].valueType(DataTypes.featureState)))

  private[HistoryWriterImpl] def isConsistent: Boolean = read { implicit l =>
    // check if all maps have same size
    Set(blockBodyByHeight().size(), blockIdByHeight().size(), heightByBlockId().size(), scoreByHeight().size()).size == 1
  }

  private def displayFeatures(s: Set[Short]): String = s"FEATURE${if (s.size > 1) "S"} ${s.mkString(", ")} ${if (s.size > 1) "WERE" else "WAS"}"

  private def updateFeaturesState(height: Int): Unit = write { implicit lock =>
    require(height % FeatureApprovalBlocksCount == 0, s"Features' state can't be updated at given height: $height")

    val acceptedFeatures = (Math.max(1, height - FeatureApprovalBlocksCount + 1) to height)
      .flatMap(h => featuresAtHeight().get(h))
      .foldLeft(Map.empty[Short, Int]) { (counts, feature) =>
        counts.updated(feature, counts.getOrElse(feature, 0) + 1)
      }.filter(p => p._2 >= MinBlocksCountToActivateFeature)
      .keys.map(k => k -> FeatureStatus.Accepted.status).toMap

    val unimplementedAccepted = acceptedFeatures.keySet.diff(BlockchainFeatures.implemented)
    if (unimplementedAccepted.nonEmpty) {
      log.warn(s"UNIMPLEMENTED ${displayFeatures(unimplementedAccepted)} ACCEPTED ON BLOCKCHAIN")
      log.warn("PLEASE, UPDATE THE NODE AS SOON AS POSSIBLE")
      log.warn("OTHERWISE THE NODE WILL BE STOPPED OR FORKED UPON FEATURE ACTIVATION")
    }

    val previousApprovalHeight = Math.max(FeatureApprovalBlocksCount, height - FeatureApprovalBlocksCount)
    val activatedFeatures: Map[Short, Byte] = Option(featuresState().get(previousApprovalHeight))
      .getOrElse(Map.empty[Short, Byte])
      .mapValues(v => if (v == FeatureStatus.Accepted.status) FeatureStatus.Activated.status else v)

    val unimplementedActivated = activatedFeatures.keySet.diff(BlockchainFeatures.implemented)
    if (unimplementedActivated.nonEmpty) {
      log.error(s"UNIMPLEMENTED ${displayFeatures(unimplementedActivated)} ACTIVATED ON BLOCKCHAIN")
      log.error("PLEASE, UPDATE THE NODE IMMEDIATELY")
      if (featuresSettings.autoShutdownOnUnsupportedFeature) {
        log.error("THE NODE WAS STOPPED AUTOMATICALLY")
        forceStopApplication(UnsupportedFeature)
      }
      else log.error("OTHERWISE THE NODE WILL END UP ON A FORK")
    }

    featuresState.mutate(_.put(height, activatedFeatures ++ acceptedFeatures))
  }

  def appendBlock(block: Block)(consensusValidation: => Either[ValidationError, BlockDiff]): Either[ValidationError, BlockDiff] =
    write { implicit lock =>

    assert(block.signatureValid)

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
      blockSizeStats.record(block.bytes.length)
      transactionsInBlockStats.record(block.transactionData.size)

      if (h % 100 == 0) db.compact(CompactFillRate, CompactMemorySize)

      log.trace(s"Full Block(id=${block.uniqueId},txs_count=${block.transactionData.size}) persisted")
      blockDiff
    }
    else {
      Left(GenericError(s"Parent ${block.reference} of block ${block.uniqueId} does not match last block ${this.lastBlock.map(_.uniqueId)}"))
    }
  }

  def discardBlock(): Seq[Transaction] = write { implicit lock =>
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

  override def lastBlockTimestamp(): Option[Long] = this.lastBlock.map(_.timestamp)

  override def lastBlockId(): Option[ByteStr] = this.lastBlock.map(_.signerData.signature)

  override def blockAt(height: Int): Option[Block] = blockBytes(height).map(Block.parseBytes(_).get)

}

object HistoryWriterImpl extends ScorexLogging {
  private val CompactFillRate = 90
  private val CompactMemorySize = 10 * 1024 * 1024

  def apply(file: Option[File], synchronizationToken: ReentrantReadWriteLock, functionalitySettings: FunctionalitySettings,
            featuresSettings: FeaturesSettings): Try[HistoryWriterImpl] =
    createWithStore[HistoryWriterImpl](file, new HistoryWriterImpl(file, synchronizationToken, functionalitySettings, featuresSettings), h => h.isConsistent)

  private val blockHeightStats = Kamon.metrics.histogram("block-height")
  private val blockSizeStats = Kamon.metrics.histogram("block-size-bytes")
  private val transactionsInBlockStats = Kamon.metrics.histogram("transactions-in-block")
}
