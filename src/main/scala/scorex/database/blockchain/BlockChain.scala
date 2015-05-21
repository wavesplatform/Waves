package scorex.database.blockchain

import scorex.account.Account
import scorex.block.Block
import settings.Settings

trait BlockChain {
  def height(): Int

  def isEmpty(): Boolean = height() == 0

  def heightOf(block: Block): Option[Int]

  def heightOf(blockSignature: Array[Byte]): Option[Int]

  def parent(block: Block): Option[Block] = heightOf(block.reference).flatMap(blockAt)

  def child(block: Block): Option[Block]

  def blockAt(height: Int): Option[Block]

  def contains(block: Block): Boolean

  def contains(signature: Array[Byte]): Boolean

  def appendBlock(block: Block): BlockChain

  def discardBlock(): BlockChain

  def lastBlock: Block = blockAt(height()).get

  def confirmations(block: Block): Option[Int] = heightOf(block).map(height() - _)

  def blockByHeader(signature: Array[Byte]): Option[Block]

  def generatedBy(account: Account): Seq[Block]

  def getSignatures(parentSignature: Array[Byte]): Seq[Array[Byte]] =
    heightOf(parentSignature).map { h =>
      (h + 1).to(Math.max(height(), h + Settings.MaxBlocksChunks)).flatMap(blockAt).map(_.signature)
    }.getOrElse(Seq())

  def lastSignatures(howMany: Int): Seq[Array[Byte]] = {
    val h = height()
    h.to(Math.max(h - howMany, 1), -1).flatMap { h =>
      blockAt(h).map(_.signature)
    }
  }

  def lastSignatures(): Seq[Array[Byte]] = lastSignatures(Settings.maxRollback)

  def lastSignature(): Array[Byte] = lastBlock.signature

  def removeAfter(signature: Array[Byte]) = while (!lastSignature().sameElements(signature)) discardBlock()

  def score = (1 to height()).foldLeft(0: BigInt) { case (score, h) =>
    score + blockAt(h).map(_.generationData.blockScore()).getOrElse(0: BigInt)
  }
}
