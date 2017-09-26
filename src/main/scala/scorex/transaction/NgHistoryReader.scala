package scorex.transaction

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.{FeatureProvider, FeatureStatus}
import com.wavesplatform.state2._
import scorex.block.Block.BlockId
import scorex.block.{Block, MicroBlock}
import scorex.transaction.History.{BlockchainScore, BlockMinerInfo}

class NgHistoryReader(ngState: () => Option[NgState], inner: History with FeatureProvider) extends History with NgHistory with DebugNgHistory with FeatureProvider {

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

  override def status(feature: Short): FeatureStatus = FeatureStatus.Defined

  override def activationHeight(feature: Short): Option[Int] = inner.activationHeight(feature)
}
