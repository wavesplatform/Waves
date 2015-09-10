package scorex.transaction

import scorex.block.{BlockProcessingModule, Block}

trait TransactionModule[TransactionBlockData] extends BlockProcessingModule[TransactionBlockData]{

  val state: State

  // todo: change to more abstract History (with possible trees support)
  val history: History

  def isValid(block: Block):Boolean

  def transactions(block: Block): Seq[Transaction]

  lazy val balancesSupport: Boolean = state match {
    case _: State with BalanceSheet => true
    case _ => false
  }

  lazy val accountWatchingSupport: Boolean = state match {
    case _: State with AccountTransactionsHistory => true
    case _ => false
  }

  def process(block: Block): Unit = state.processBlock(block, reversal = false)

  def popOff(block: Block): Unit = state.processBlock(block, reversal = true)

  def packUnconfirmed(): TransactionBlockData

  def clearFromUnconfirmed(data: TransactionBlockData): Unit
}