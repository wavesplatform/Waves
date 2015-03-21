package scorex.database

import java.util.concurrent.atomic.AtomicReference

import scorex.account.Account
import scorex.block.Block
import scorex.transaction.Transaction


object PrunableBlockchainStorage extends BlockChain {
  val chainAfterSnapshot: AtomicReference[BlockChain] = new AtomicReference(new YoctoBlockchainImpl)
  var snapshot: Option[Snapshot] = None

  override def height(): Int = chainAfterSnapshot.get().height()

  override def appendBlock(block: Block): BlockChain = {
    chainAfterSnapshot.set(chainAfterSnapshot.get().appendBlock(block))
    chainAfterSnapshot.get()
  }

  override def discardBlock(): BlockChain = {
    chainAfterSnapshot.set(chainAfterSnapshot.get().discardBlock())
    chainAfterSnapshot.get()
  }

  override def heightOf(block: Block): Option[Int] = chainAfterSnapshot.get().heightOf(block)

  override def heightOf(blockSignature: Array[Byte]): Option[Int] = chainAfterSnapshot.get().heightOf(blockSignature)

  override def blockAt(height: Int): Option[Block] = chainAfterSnapshot.get().blockAt(height)

  override def contains(block: Block): Boolean = chainAfterSnapshot.get().contains(block)

  override def balance(address: String, fromHeight: Int, confirmations: Int): BigDecimal = {
    chainAfterSnapshot.get().balance(address, fromHeight, confirmations)
  }

  override def child(block: Block): Option[Block] = chainAfterSnapshot.get().child(block)

  override def confirmations(tx: Transaction): Option[Int] = chainAfterSnapshot.get().confirmations(tx)

  override def blockByHeader(signature: Array[Byte]): Option[Block] = chainAfterSnapshot.get().blockByHeader(signature)

  def generatedBy(account: Account): Seq[Block] = chainAfterSnapshot.get().generatedBy(account)

  override def accountTransactions(account: Account): Seq[Transaction] =
    chainAfterSnapshot.get().accountTransactions(account)

}
