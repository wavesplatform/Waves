package scorex.transaction

import scorex.block.Block
import scorex.utils.ScorexLogging

trait BlockChain extends History with ScorexLogging {

  def blockAt(height: Int): Option[Block]

  def genesisBlock = blockAt(1)

  override def parent(block: Block): Option[Block] =
    heightOf(block.referenceField.value).flatMap(blockAt)

  def discardBlock(): BlockChain

  def lastBlock: Block = blockAt(height()).get

  def getSignatures(parentSignature: Block.BlockId, howMany: Int): Seq[Block.BlockId] =
    heightOf(parentSignature).map { h =>
      (h + 1).to(Math.min(height(), h + howMany)).flatMap(blockAt).map(_.uniqueId)
    }.getOrElse(Seq())

  override def lastSignatures(howMany: Int): Seq[Block.BlockId] = {
    val h = height()
    h.to(Math.max(h - howMany, 1), -1).flatMap { h =>
      blockAt(h).map(_.uniqueId)
    }
  }

  def lastSignature(): Block.BlockId = lastBlock.uniqueId

  override def removeAfter(signature: Block.BlockId) = while (!lastSignature().sameElements(signature)) discardBlock()

  override def score(): History.BlockchainScore =
    (1 to height()).foldLeft(0: BigInt) { case (sc, h) =>
      sc + blockAt(h).map { bl: Block =>
        bl.consensusModule.blockScore(bl)(bl.transactionModule)
      }.getOrElse(0: BigInt)
    }
}