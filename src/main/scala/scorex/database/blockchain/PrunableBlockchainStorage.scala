package scorex.database.blockchain

import scorex.account.Account
import scorex.block.Block
import scorex.transaction.Transaction

//todo: object isn't thread-safe!

object PrunableBlockchainStorage extends BlockChain {
  private var chainAfterSnapshot: BlockChain = new YoctoBlockchainImpl
  private var snapshot: Option[Snapshot] = None

  override def height(): Int = chainAfterSnapshot.height()

  override def appendBlock(block: Block): BlockChain = {
    chainAfterSnapshot = chainAfterSnapshot.appendBlock(block)
    chainAfterSnapshot
  }

  override def discardBlock(): BlockChain = {
    chainAfterSnapshot = chainAfterSnapshot.discardBlock()
    chainAfterSnapshot
  }

  override def heightOf(block: Block): Option[Int] = chainAfterSnapshot.heightOf(block)

  override def heightOf(blockSignature: Array[Byte]): Option[Int] = chainAfterSnapshot.heightOf(blockSignature)

  override def blockAt(height: Int): Option[Block] = chainAfterSnapshot.blockAt(height)

  override def contains(block: Block): Boolean = chainAfterSnapshot.contains(block)

  override def contains(signature: Array[Byte]): Boolean = chainAfterSnapshot.contains(signature)

  override def balance(address: String, confirmations: Int): BigDecimal = {
    chainAfterSnapshot.balance(address, confirmations)
  }

  override def child(block: Block): Option[Block] = chainAfterSnapshot.child(block)

  override def confirmations(tx: Transaction): Option[Int] = chainAfterSnapshot.confirmations(tx)

  override def blockByHeader(signature: Array[Byte]): Option[Block] = chainAfterSnapshot.blockByHeader(signature)

  def generatedBy(account: Account): Seq[Block] = chainAfterSnapshot.generatedBy(account)

  override def accountTransactions(account: Account): Seq[Transaction] =
    chainAfterSnapshot.accountTransactions(account)
}