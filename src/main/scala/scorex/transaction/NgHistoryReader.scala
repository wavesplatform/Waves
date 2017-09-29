package scorex.transaction

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.{FeatureProvider, BlockchainFeatureStatus}
import com.wavesplatform.state2._
import scorex.block.Block.BlockId
import scorex.block.{Block, MicroBlock}
import scorex.transaction.History.{BlockchainScore, BlockMinerInfo}

class NgHistoryReader(ngState: () => Option[NgState], inner: History with FeatureProvider) extends History with NgHistory with DebugNgHistory with FeatureProvider {

  override val ActivationWindowSize: Int = inner.ActivationWindowSize
  override val MinVotesWithinWindowToActivateFeature: Int = inner.MinVotesWithinWindowToActivateFeature

  override def synchronizationToken: ReentrantReadWriteLock = inner.synchronizationToken

  override def height(): Int = read { implicit l =>
    inner.height() + ngState().size
  }

  override def blockBytes(height: Int): Option[Array[Byte]] = read { implicit l =>
    inner.blockBytes(height).orElse(if (height == inner.height() + 1) ngState().map(_.bestLiquidBlock.bytes) else None)
  }

  override def scoreOf(blockId: BlockId): Option[BlockchainScore] = read { implicit l =>
    inner.scoreOf(blockId)
      .orElse(ngState() match {
        case Some(ng) if ng.contains(blockId) => Some(inner.score() + ng.base.blockScore)
        case _ => None
      })
  }

  override def heightOf(blockId: BlockId): Option[Int] = read { implicit l =>
    lazy val innerHeight = inner.height()
    inner.heightOf(blockId).orElse(ngState() match {
      case Some(ng) if ng.contains(blockId) => Some(innerHeight + 1)
      case _ => None
    })
  }

  override def lastBlockIds(howMany: Int): Seq[BlockId] = read { implicit l =>
    ngState() match {
      case Some(ng) =>
        ng.bestLiquidBlockId +: inner.lastBlockIds(howMany - 1)
      case None =>
        inner.lastBlockIds(howMany)
    }
  }

  override def microBlock(id: BlockId): Option[MicroBlock] = read { implicit l =>
    for {
      ng <- ngState()
      mb <- ng.micros.find(_.totalResBlockSig == id)
    } yield mb
  }

  override def close(): Unit = inner.close()

  override def lastBlockTimestamp(): Option[Long] = read { implicit l =>
    ngState().map(_.base.timestamp).orElse(inner.lastBlockTimestamp())
  }

  override def lastBlockId(): Option[AssetId] = read { implicit l =>
    ngState().map(_.bestLiquidBlockId).orElse(inner.lastBlockId())
  }

  override def blockAt(height: Int): Option[Block] = read { implicit l =>
    if (height == inner.height() + 1)
      ngState().map(_.bestLiquidBlock)
    else
      inner.blockAt(height)
  }

  override def lastPersistedBlockIds(count: Int): Seq[BlockId] = read { implicit l =>
    inner.lastBlockIds(count)
  }

  override def microblockIds(): Seq[BlockId] =
    ngState().toSeq.flatMap(ng => ng.micros.map(_.totalResBlockSig))

  override def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo] = read { implicit l =>
    ngState().map(_.bestLastBlockInfo(maxTimestamp))
      .orElse(inner.lastBlock.map(b => BlockMinerInfo(b.consensusData, b.timestamp, b.uniqueId)))
  }

  override def featureVotesCountWithinActivationWindow(height: Int): Map[Short, Int] = {
    val votes = inner.featureVotesCountWithinActivationWindow(height)
    if (activationWindowOpeningFromHeight(this.height()) == activationWindowOpeningFromHeight(height))
      ngState().map(_.base.supportedFeaturesIds).getOrElse(Seq.empty).foldLeft(votes)((votes, f) => votes + (f -> (votes.getOrElse(f, 0) + 1)))
    else votes
  }

  override def activationWindowOpeningFromHeight(height: Int): Int = inner.activationWindowOpeningFromHeight(height)

  override def featureStatus(feature: Short): BlockchainFeatureStatus = {
    val status = inner.featureStatus(feature)
    if (height() % ActivationWindowSize != 0) status
    else {
      val ngVote = ngState().map(_.base.supportedFeaturesIds.count(_ == feature)).getOrElse(0)
      if (inner.featureVotesCountWithinActivationWindow(height()).getOrElse(feature, 0) + ngVote >= MinVotesWithinWindowToActivateFeature)
        BlockchainFeatureStatus.promote(status) else status
    }
  }

  override def featureActivationHeight(feature: Short): Option[Int] =
    inner.featureActivationHeight(feature).orElse {
      featureStatus(feature) match {
        case BlockchainFeatureStatus.Activated => Some(height())
        case BlockchainFeatureStatus.Accepted => Some(height() + ActivationWindowSize)
        case BlockchainFeatureStatus.Undefined => None
      }
    }
}
