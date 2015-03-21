package scorex.database

import scorex.account.Account
import scorex.block.Block
import scorex.transaction.Transaction
import settings.Settings

trait BlockChain extends StateQuery {
  def height(): Int

  def isEmpty(): Boolean = height() == 0

  def heightOf(block: Block): Option[Int]

  def heightOf(blockSignature: Array[Byte]): Option[Int]

  def parent(block: Block): Option[Block] = heightOf(block).flatMap(h => blockAt(h - 1))

  def child(block: Block): Option[Block]

  def blockAt(height: Int): Option[Block]

  def contains(block: Block): Boolean

  def appendBlock(block: Block): BlockChain

  def discardBlock(): BlockChain

  def lastBlock: Block = blockAt(height()).get

  def confirmations(tx: Transaction): Option[Int]

  def confirmations(block: Block): Option[Int] = heightOf(block).map(height() - _)

  def blockByHeader(signature: Array[Byte]): Option[Block]

  def generatedBy(account: Account): Seq[Block]

  def getSignatures(parentSignature: Array[Byte]): Seq[Array[Byte]] =
    heightOf(parentSignature).map { h =>
      (h + 1).to(Math.max(height(), h + Settings.MaxBlocksChunks)).flatMap(blockAt).map(_.signature)
    }.getOrElse(Seq())
}
