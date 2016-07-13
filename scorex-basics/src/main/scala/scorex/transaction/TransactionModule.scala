package scorex.transaction

import scorex.block.{Block, BlockProcessingModule}

trait TransactionModule[TransactionBlockData] extends BlockProcessingModule[TransactionBlockData] {

  val blockStorage: BlockStorage

  val utxStorage: UnconfirmedTransactionsStorage

  def isValid(block: Block): Boolean

  /**
    * Check whether tx is valid on current state
    */
  def isValid(tx: Transaction): Boolean = blockStorage.state.isValid(tx)

  def transactions(block: Block): Seq[Transaction]

  /**
    * Returns all unconfirmed transactions
    */
  def unconfirmedTxs() : Seq[Transaction] = utxStorage.all()

  def putUnconfirmedIfNew(tx: Transaction): Boolean = utxStorage.putIfNew(tx)

  def packUnconfirmed(): TransactionBlockData

  def clearFromUnconfirmed(data: TransactionBlockData): Unit

  def onNewOffchainTransaction(transaction: Transaction): Unit

  lazy val balancesSupport: Boolean = blockStorage.state match {
    case _: State with BalanceSheet => true
    case _ => false
  }

  lazy val accountWatchingSupport: Boolean = blockStorage.state match {
    case _: State with AccountTransactionsHistory => true
    case _ => false
  }
}
