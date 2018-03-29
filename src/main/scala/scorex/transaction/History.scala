package scorex.transaction

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.state2.ByteStr
import scorex.account.AddressScheme
import scorex.block.Block.BlockId
import scorex.block.{Block, BlockHeader, MicroBlock}
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction.History.{BlockMinerInfo, BlockchainScore}

trait History extends FeatureProvider {
  def height: Int
  def score: BlockchainScore
  def scoreOf(blockId: ByteStr): Option[BlockchainScore]

  def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)]
  def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)]

  def lastBlock: Option[Block]
  def blockBytes(height: Int): Option[Array[Byte]]
  def blockBytes(blockId: ByteStr): Option[Array[Byte]]

  def heightOf(blockId: ByteStr): Option[Int]

  /** Returns the most recent block IDs, starting from the most recent  one */
  def lastBlockIds(howMany: Int): Seq[ByteStr]

  /** Returns a chain of blocks starting with the block with the given ID (from oldest to newest) */
  def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]]

  def parent(block: Block, back: Int = 1): Option[Block]
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
        _.items.exists {
          case BlockCheckpoint(h, sig) =>
            h == estimatedHeight && candidateSignature != ByteStr(sig)
        }
      }
  }

}

object History {

  type BlockchainScore = BigInt

  case class BlockMinerInfo(consensus: NxtLikeConsensusBlockData, timestamp: Long, blockId: BlockId)

  implicit class HistoryExt(history: History) {
    def isEmpty: Boolean = history.height == 0

    def contains(block: Block): Boolean       = history.contains(block.uniqueId)
    def contains(signature: ByteStr): Boolean = history.heightOf(signature).isDefined

    def blockById(blockId: ByteStr)(implicit addressScheme: AddressScheme): Option[Block] =
      history.blockBytes(blockId).flatMap(bb => Block.parseBytes(bb).toOption)
    def blockAt(height: Int)(implicit addressScheme: AddressScheme): Option[Block] =
      history.blockBytes(height).flatMap(bb => Block.parseBytes(bb).toOption)

    def lastBlockHeaderAndSize: Option[(Block, Int)] = history.lastBlock.map(b => (b, b.bytes().length))
    def lastBlockId: Option[AssetId]                 = history.lastBlockHeaderAndSize.map(_._1.signerData.signature)
    def lastBlockTimestamp: Option[Long]             = history.lastBlockHeaderAndSize.map(_._1.timestamp)

    def lastBlocks(howMany: Int)(implicit addressScheme: AddressScheme): Seq[Block] = {
      (Math.max(1, history.height - howMany + 1) to history.height).flatMap(h => history.blockAt(h)).reverse
    }

    def genesis(implicit addressScheme: AddressScheme): Block = history.blockAt(1).get
  }
}
