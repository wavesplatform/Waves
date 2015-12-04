package scorex.transaction

import scorex.block.Block
import scorex.utils.ScorexLogging

import scala.util.Try

trait BlockChain extends History with ScorexLogging {

  def blockAt(height: Int): Option[Block]

  def genesisBlock = blockAt(1)

  override def parent(block: Block): Option[Block] =
    heightOf(block.referenceField.value).flatMap(blockAt)

  private[transaction] def discardBlock(): BlockChain

  override def lastBlock: Block = blockAt(height()).get

  def getSignatures(parentSignature: Block.BlockId, howMany: Int): Seq[Block.BlockId] =
    heightOf(parentSignature).map { h =>
      (h + 1).to(Math.min(height(), h + howMany: Int)).flatMap(blockAt).map(_.uniqueId)
    }.getOrElse(Seq())

  def lastSignatures(howMany: Int): Seq[Block.BlockId] = {
    val h = height()
    h.to(Math.max(h - howMany, 1), -1).flatMap { h =>
      blockAt(h).map(_.uniqueId)
    }
  }

  def children(block: Block): Seq[Block]

  /**
    * Average delay in milliseconds between last $blockNum blocks starting from $block
    */
  def averageDelay(block: Block, blockNum: Int): Try[Long] = Try {
    val height: Int = heightOf(block).get
    (blockAt(height).get.timestampField.value - blockAt(height - blockNum).get.timestampField.value) / blockNum
  }

  def lastSignature(): Block.BlockId = lastBlock.uniqueId

  def score() =
    (1 to height()).foldLeft(0: BigInt) { case (sc, h) =>
      sc + blockAt(h).map { bl: Block =>
        bl.consensusModule.blockScore(bl)(bl.transactionModule)
      }.getOrElse(0: BigInt)
    }
}