package scorex.transaction

import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.state2.ByteStr
import scorex.block.Block.BlockId
import scorex.block.{Block, BlockHeader, MicroBlock}
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction.History.{BlockMinerInfo, BlockchainScore}
import scorex.utils.Synchronized

import scala.util.Try

trait History extends Synchronized with AutoCloseable {

  def height(): Int

  def blockAt(height: Int): Option[Block]

  def blockHeaderAndSizeAt(height: Int): Option[(BlockHeader, Int)]

  def blockBytes(height: Int): Option[Array[Byte]]

  def scoreOf(id: ByteStr): Option[BlockchainScore]

  def heightOf(blockId: ByteStr): Option[Int]

  def lastBlockIds(howMany: Int): Seq[ByteStr]

  def lastBlockTimestamp(): Option[Long]

  def lastBlockId(): Option[ByteStr]
}

trait NgHistory extends History {
  def microBlock(id: ByteStr): Option[MicroBlock]

  def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo]
}

trait DebugNgHistory {
  def lastPersistedBlockIds(count: Int): Seq[BlockId]

  def microblockIds(): Seq[BlockId]
}

trait CheckpointService {

  def set(checkpoint: Checkpoint): Either[ValidationError, Unit]

  def get: Option[Checkpoint]
}

object CheckpointService {

  implicit class CheckpointServiceExt(cs: CheckpointService) {
    def isBlockValid(candidateSignature: ByteStr, estimatedHeight: Int): Boolean =
      !cs.get.exists {
        _.items.exists { case BlockCheckpoint(h, sig) =>
          h == estimatedHeight && candidateSignature != ByteStr(sig)
        }
      }
  }

}

object History {

  type BlockchainScore = BigInt

  case class BlockMinerInfo(consensus: NxtLikeConsensusBlockData, timestamp: Long, blockId: BlockId)

  implicit class HistoryExt(history: History) {

    def score(): BlockchainScore = history.read { implicit lock =>
      history.lastBlock.flatMap(last => history.scoreOf(last.uniqueId)).getOrElse(0)
    }

    def isEmpty: Boolean = history.height() == 0

    def contains(block: Block): Boolean = history.contains(block.uniqueId)

    def contains(signature: ByteStr): Boolean = history.heightOf(signature).isDefined

    def blockById(blockId: ByteStr): Option[Block] = history.read { _ =>
      history.heightOf(blockId).flatMap(history.blockAt)
    }

    def blockById(blockId: String): Option[Block] = ByteStr.decodeBase58(blockId).toOption.flatMap(history.blockById)

    def heightOf(block: Block): Option[Int] = history.heightOf(block.uniqueId)

    def confirmations(block: Block): Option[Int] = history.read { _ =>
      heightOf(block).map(history.height() - _)
    }

    def lastBlock: Option[Block] = history.read { _ =>
      history.blockAt(history.height())
    }

    def averageDelay(block: Block, blockNum: Int): Try[Long] = Try {
      (block.timestamp - parent(block, blockNum).get.timestamp) / blockNum
    }

    def parent(block: Block, back: Int = 1): Option[Block] = history.read { _ =>
      require(back > 0)
      history.heightOf(block.reference).flatMap(referenceHeight => history.blockAt(referenceHeight - back + 1))
    }

    def child(block: Block): Option[Block] = history.read { _ =>
      history.heightOf(block.uniqueId).flatMap(h => history.blockAt(h + 1))
    }

    def lastBlocks(howMany: Int): Seq[Block] = history.read { _ =>
      (Math.max(1, history.height() - howMany + 1) to history.height()).flatMap(history.blockAt).reverse
    }

    def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Seq[ByteStr] = history.read { _ =>
      history.heightOf(parentSignature).map { h =>
        (h + 1).to(Math.min(history.height(), h + howMany: Int)).flatMap(history.blockAt).map(_.uniqueId)
      }.getOrElse(Seq())
    }

    def genesis: Block = history.blockAt(1).get
  }
}
