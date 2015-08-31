package scorex.transaction

import scorex.block.{Block, BlockField}


trait TransactionModule[TransactionBlockData] {

  val state: State

  // todo: change to more abstract History (with possible trees support)
  val history: History

  def isValid(block: Block):Boolean

  def parseBlockData(bytes: Array[Byte]): BlockField[TransactionBlockData]

  def parseBlockFields(blockFields: BlockField[TransactionBlockData]): TransactionBlockData =
    blockFields.value

  def genesisData:BlockField[TransactionBlockData]

  def transactions(block: Block): Seq[Transaction]

  def balancesSupport(): Boolean = state match {
    case _: State with BalanceSheet => true
    case _ => false
  }

  def accountWatchingSupport(): Boolean = state match {
    case _: State with AccountTransactionsHistory => true
    case _ => false
  }

  def process(block: Block): Unit = state.processBlock(block, reversal = false)

  def popOff(block: Block): Unit = state.processBlock(block, reversal = true)

  def formBlockData(data: TransactionBlockData): BlockField[TransactionBlockData]

  def packUnconfirmed(): TransactionBlockData

  def clearFromUnconfirmed(data: TransactionBlockData): Unit
}