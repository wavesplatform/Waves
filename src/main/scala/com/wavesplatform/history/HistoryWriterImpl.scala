package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.{FeaturesSettings, FunctionalitySettings}
import com.wavesplatform.state2.{BlockDiff, ByteStr, DataTypes, VariablesStorage, VersionableStorage}
import com.wavesplatform.utils._
import kamon.Kamon
import scorex.block.{Block, BlockHeader}
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.utils.{LogMVMapBuilder, ScorexLogging}

import scala.collection.JavaConverters._
import scala.util.Try

class HistoryWriterImpl private(file: Option[File], val synchronizationToken: ReentrantReadWriteLock,
                                functionalitySettings: FunctionalitySettings, featuresSettings: FeaturesSettings)
  extends VariablesStorage with VersionableStorage with History with FeatureProvider with ScorexLogging {

  override protected val Version: Int = 1

  import HistoryWriterImpl._

  override val activationWindowSize: Int = functionalitySettings.featureCheckBlocksPeriod
  val MinVotesWithinWindowToActivateFeature: Int = functionalitySettings.blocksForFeatureActivation

  private val blockBodyByHeight = Synchronized(db.openMap("blocks", new LogMVMapBuilder[Int, Array[Byte]]))
  private val blockIdByHeight = Synchronized(db.openMap("signatures", new LogMVMapBuilder[Int, ByteStr].valueType(DataTypes.byteStr)))
  private val heightByBlockId = Synchronized(db.openMap("signaturesReverse", new LogMVMapBuilder[ByteStr, Int].keyType(DataTypes.byteStr)))
  private val scoreByHeight = Synchronized(db.openMap("score", new LogMVMapBuilder[Int, BigInt]))
  private val featuresVotes = Synchronized(db.openMap("features-votes", new LogMVMapBuilder[Int, Map[Short, Int]].valueType(DataTypes.mapShortInt)))
  private val featuresState = Synchronized(db.openMap("features-state", new LogMVMapBuilder[Short, Int]))

  private[HistoryWriterImpl] def isConsistent: Boolean = read { implicit l =>
    // check if all maps have same size
    Set(blockBodyByHeight().size(), blockIdByHeight().size(), heightByBlockId().size(), scoreByHeight().size()).size == 1
  }

  private lazy val preAcceptedFeatures = functionalitySettings.preActivatedFeatures.mapValues(h => h - activationWindowSize)

  override def approvedFeatures(): Map[Short, Int] = read { implicit lock =>
    preAcceptedFeatures ++ featuresState().asScala
  }

  override def featureVotesCountWithinActivationWindow(height: Int): Map[Short, Int] = read { implicit lock =>
    featuresVotes().getOrDefault(FeatureProvider.votingWindowOpeningFromHeight(height, activationWindowSize), Map.empty)
  }

  private def alterVotes(height: Int, votes: Set[Short], voteMod: Int): Unit = write { implicit lock =>
    val votingWindowOpening = FeatureProvider.votingWindowOpeningFromHeight(height, activationWindowSize)
    val votesWithinWindow = featuresVotes().getOrDefault(votingWindowOpening, Map.empty[Short, Int])
    val newVotes = votes.foldLeft(votesWithinWindow)((v, feature) => v + (feature -> (v.getOrElse(feature, 0) + voteMod)))
    featuresVotes.mutate(_.put(votingWindowOpening, newVotes))
  }

  def appendBlock(block: Block, acceptedFeatures: Set[Short])(consensusValidation: => Either[ValidationError, BlockDiff]): Either[ValidationError, BlockDiff] =
    write { implicit lock =>

      assert(block.signaturesValid().isRight)

      if ((height() == 0) || (this.lastBlock.get.uniqueId == block.reference)) consensusValidation.map { blockDiff =>
        val h = height() + 1
        val score = (if (height() == 0) BigInt(0) else this.score()) + block.blockScore()
        blockBodyByHeight.mutate(_.put(h, block.bytes()))
        scoreByHeight.mutate(_.put(h, score))
        blockIdByHeight.mutate(_.put(h, block.uniqueId))
        heightByBlockId.mutate(_.put(block.uniqueId, h))
        featuresState.mutate(_.putAll(acceptedFeatures.diff(featuresState().keySet.asScala).map(_ -> h).toMap.asJava))
        alterVotes(h, block.featureVotes, 1)
        db.commit()
        blockHeightStats.record(h)
        blockSizeStats.record(block.bytes().length)
        transactionsInBlockStats.record(block.transactionData.size)

        if (h % 100 == 0) db.compact(CompactFillRate, CompactMemorySize)

        blockDiff
      }
      else {
        Left(GenericError(s"Parent ${block.reference} of block ${block.uniqueId} does not match last block ${this.lastBlock.map(_.uniqueId)}"))
      }
    }

  def discardBlock(): Option[Block] = write { implicit lock =>
    val h = height()

    alterVotes(h, blockAt(h).map(b => b.featureVotes).getOrElse(Set.empty), -1)

    val removedBlockBytes = blockBodyByHeight.mutate(_.remove(h))
    val maybeDiscardedBlock = Block.parseBytes(removedBlockBytes).toOption
    scoreByHeight.mutate(_.remove(h))

    if (h % activationWindowSize == 0) {
      val featuresToRemove = featuresState().entrySet().asScala.filter(_.getValue == h).map(_.getKey)
      featuresState.mutate(fs => featuresToRemove.foreach(fs.remove))
    }

    val vOpt = Option(blockIdByHeight.mutate(_.remove(h)))
    vOpt.map(v => heightByBlockId.mutate(_.remove(v)))
    db.commit()

    maybeDiscardedBlock
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

  override def lastBlockTimestamp(): Option[Long] = this.lastBlock.map(_.timestamp)

  override def lastBlockId(): Option[ByteStr] = this.lastBlock.map(_.signerData.signature)

  override def blockAt(height: Int): Option[Block] = blockBytes(height).map(Block.parseBytes(_).get)

  override def blockHeaderAndSizeAt(height: Int): Option[(BlockHeader, Int)] =
    blockBytes(height).map(bytes => (BlockHeader.parseBytes(bytes).get._1, bytes.length))
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
