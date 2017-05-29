package scorex.transaction

import com.wavesplatform.network.Checkpoint
import com.wavesplatform.state2.ByteStr
import scorex.account.Account
import scorex.block.Block
import scorex.network.Checkpoint
import scorex.transaction.History.BlockchainScore
import scorex.utils.Synchronized

import scala.util.Try

trait History extends Synchronized {

  def height(): Int

  def blockAt(height: Int): Option[Block]

  def blockBytes (height: Int): Option[Array[Byte]]

  def score(): BlockchainScore

  def scoreOf(id: ByteStr): BlockchainScore

  def heightOf(blockId: ByteStr): Option[Int]

  def generatedBy(account: Account, from: Int, to: Int): Seq[Block]

  def lastBlockIds(howMany: Int): Seq[ByteStr]
}

trait HistoryWriter extends History {

  def appendBlock(block: Block): Either[ValidationError, Unit]

  def discardBlock(): Unit
}

trait CheckpointService {

  def set(checkpoint: Option[Checkpoint])

  def get: Option[Checkpoint]
}


object History {
  type BlockchainScore = BigInt

  implicit class HistoryExt(history: History) {
    def isEmpty: Boolean = history.height() == 0

    def contains(block: Block): Boolean = history.contains(block.uniqueId)

    def contains(signature: ByteStr): Boolean = history.heightOf(signature).isDefined

    def blockById(blockId: ByteStr): Option[Block] = history.read { implicit lock =>
      history.heightOf(blockId).flatMap(history.blockAt)
    }

    def blockById(blockId: String): Option[Block] = ByteStr.decodeBase58(blockId).toOption.flatMap(history.blockById)

    def heightOf(block: Block): Option[Int] = history.heightOf(block.uniqueId)

    def confirmations(block: Block): Option[Int] = history.read { implicit lock =>
      heightOf(block).map(history.height() - _)
    }

    def lastBlock: Block = history.read { implicit lock =>
      history.blockAt(history.height()).get
    }

    def averageDelay(block: Block, blockNum: Int): Try[Long] = Try {
      (block.timestamp - parent(block, blockNum).get.timestamp) / blockNum
    }

    def parent(block: Block, back: Int = 1): Option[Block] = history.read { implicit lock =>
      require(back > 0)
      history.heightOf(block.reference).flatMap(referenceHeight => history.blockAt(referenceHeight - back + 1))
    }

    def child(block: Block): Option[Block] = history.read { implicit lock =>
      history.heightOf(block.uniqueId).flatMap(h => history.blockAt(h + 1))
    }

    def lastBlocks(howMany: Int): Seq[Block] = history.read { implicit lock =>
      (Math.max(1, history.height() - howMany + 1) to history.height()).flatMap(history.blockAt).reverse
    }

    def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Seq[ByteStr] = history.read { implicit lock =>
      history.heightOf(parentSignature).map { h =>
        (h + 1).to(Math.min(history.height(), h + howMany: Int)).flatMap(history.blockAt).map(_.uniqueId)
      }.getOrElse(Seq())
    }

    def genesis: Block = history.blockAt(1).get
  }

}
