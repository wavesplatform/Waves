package scorex.database.blockchain

import java.io.File
import scorex.account.Account
import scorex.block.Block
import scorex.transaction._
import settings.Settings
import scala.collection.concurrent.TrieMap


class BlockchainImpl extends BlockChain {

  private val signaturesIndex = TrieMap[Int, Array[Byte]]()
  private val blocksIndex = TrieMap[Int, Block]()

  override def appendBlock(block: Block): BlockChain = {
    val h = height() + 1
    signaturesIndex += h -> block.signature
    blocksIndex += h -> block
    this
  }.ensuring(_ => signaturesIndex.size == blocksIndex.size)

  override def heightOf(block: Block): Option[Int] = signaturesIndex.find(_._2.sameElements(block.signature)).map(_._1)

  override def blockAt(height: Int): Option[Block] = blocksIndex.get(height)

  override def contains(block: Block): Boolean = contains(block.signature)

  override def contains(signature:Array[Byte]): Boolean = signaturesIndex.exists(_._2.sameElements(signature))

  override def height(): Int = signaturesIndex.size

  private def filename(height: Int) = Settings.dataDir + s"/block-${height + 1}"

  override def heightOf(blockSignature: Array[Byte]): Option[Int] =
    signaturesIndex.find(_._2.sameElements(blockSignature)).map(_._1)

  override def blockByHeader(signature: Array[Byte]): Option[Block] =
    signaturesIndex.find(_._2.sameElements(signature)).map(_._1).map(h => blocksIndex(h))

  //todo: implement
  override def confirmations(tx: Transaction): Option[Int] = ???

  override def discardBlock(): BlockChain = {
    require(height() > 1, "Chain is empty or contains genesis block only")
    val key = height()
    signaturesIndex -= key
    blocksIndex -= key
    new File(filename(key)).delete()
    this
  }.ensuring(_ => signaturesIndex.size == blocksIndex.size)

  //todo: implement
  override def child(block: Block): Option[Block] = ???

  //todo: implement
  override def generatedBy(account: Account): Seq[Block] = ???
}