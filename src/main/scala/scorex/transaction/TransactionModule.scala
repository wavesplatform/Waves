package scorex.transaction

import scorex.block.{Block, BlockField, BlockProcessingModule}

import scala.util.Try

trait TransactionModule {

  def genesisData: Seq[Transaction]

  val blockStorage: BlockStorage

  def isValid(block: Block): Boolean

  def isValid(tx: Transaction, blockTime: Long): Boolean

  def unconfirmedTxs: Seq[Transaction]

  def putUnconfirmedIfNew(tx: Transaction): Boolean

  def packUnconfirmed(): Seq[Transaction]

  def clearFromUnconfirmed(data: Seq[Transaction]): Unit

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
