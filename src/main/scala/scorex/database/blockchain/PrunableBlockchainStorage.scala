package scorex.database.blockchain

import scorex.account.Account
import scorex.block.Block
import scorex.transaction.Transaction

//todo: object isn't thread-safe!

/**
 * Facade to both blockchain & internal state implementations
 */
object PrunableBlockchainStorage extends BlockChain with StateQuery {
  private val chain = new BlockchainImpl()
  private val state = new InternalState()

  override def height(): Int = chain.height()

  override def appendBlock(block: Block): BlockChain = {
    state.appendBlock(block)
    chain.appendBlock(block)
    chain
  }

  override def discardBlock(): BlockChain = {
    state.discardBlock(chain.lastBlock)
    chain.discardBlock()
    chain
  }

  //chain functions
  override def heightOf(block: Block): Option[Int] = chain.heightOf(block)

  override def heightOf(blockSignature: Array[Byte]): Option[Int] = chain.heightOf(blockSignature)

  override def blockAt(height: Int): Option[Block] = chain.blockAt(height)

  override def contains(block: Block): Boolean = chain.contains(block)

  override def contains(signature: Array[Byte]): Boolean = chain.contains(signature)

  override def child(block: Block): Option[Block] = chain.child(block)

  override def blockByHeader(signature: Array[Byte]): Option[Block] = chain.blockByHeader(signature)

  def generatedBy(account: Account): Seq[Block] = chain.generatedBy(account)

  //state functions

  override def balance(address: String, confirmations: Int): BigDecimal = state.balance(address, confirmations)

  override def accountTransactions(account: Account): Seq[Transaction] = state.accountTransactions(account)

  override def watchAccountTransactions(account: Account): Unit = state.watchAccountTransactions(account)

  override def stopWatchingAccountTransactions(account: Account): Unit = state.stopWatchingAccountTransactions(account)
}