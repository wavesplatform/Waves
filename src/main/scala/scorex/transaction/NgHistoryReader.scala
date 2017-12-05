package scorex.transaction

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import scorex.block.Block.BlockId
import scorex.block.{Block, BlockHeader, MicroBlock}
import scorex.transaction.History.{BlockMinerInfo, BlockchainScore}
import cats.implicits._

class NgHistoryReader(ngState: () => Option[NgState], inner: History with FeatureProvider, settings: FunctionalitySettings) extends History with NgHistory with DebugNgHistory with FeatureProvider {

  override val activationWindowSize: Int = settings.featureCheckBlocksPeriod

  override def synchronizationToken: ReentrantReadWriteLock = inner.synchronizationToken

  override def height(): Int = read { implicit l =>
    inner.height() + ngState().map(_ => 1).getOrElse(0)
  }

  override def blockBytes(height: Int): Option[Array[Byte]] = read { implicit l =>
    inner.blockBytes(height).orElse(if (height == inner.height() + 1) ngState().map(_.bestLiquidBlock.bytes()) else None)
  }

  override def scoreOf(blockId: BlockId): Option[BlockchainScore] = read { implicit l =>
    inner.scoreOf(blockId)
      .orElse(ngState() match {
        case Some(ng) if ng.contains(blockId) => Some(inner.score() + ng.base.blockScore())
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
      mb <- ng.microBlock(id)
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

  override def microblockIds(): Seq[BlockId] = read { implicit l =>
    ngState().toSeq.flatMap(_.microBlockIds)
  }

  override def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo] = read { implicit l =>
    ngState().map(_.bestLastBlockInfo(maxTimestamp))
      .orElse(inner.lastBlock.map(b => BlockMinerInfo(b.consensusData, b.timestamp, b.uniqueId)))
  }

  override def approvedFeatures(): Map[Short, Int] = {
    lazy val h = height()
    ngState().map(_.acceptedFeatures.map(_ -> h).toMap).getOrElse(Map.empty) ++ inner.approvedFeatures()
  }

  override def featureVotesCountWithinActivationWindow(height: Int): Map[Short, Int] = read { implicit l =>
    val ngVotes = ngState().map(_.base.featureVotes.map(_ -> 1).toMap).getOrElse(Map.empty)
    inner.featureVotesCountWithinActivationWindow(height) |+| ngVotes
  }

  override def blockHeaderAndSizeAt(height: Int): Option[(BlockHeader, Int)] = read { implicit l =>
    if (height == inner.height() + 1)
      ngState().map(x => (x.bestLiquidBlock, x.bestLiquidBlock.bytes().length))
    else
      inner.blockHeaderAndSizeAt(height)
  }
}
