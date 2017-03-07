package scorex.transaction

import scorex.block.Block

trait TransactionModule {

  def genesisData: Seq[Transaction]

  val blockStorage: BlockStorage

  def isValid(block: Block): Boolean

  def validate[T <: Transaction](tx: T): Either[ValidationError, T]

  def unconfirmedTxs: Seq[Transaction]

  def putUnconfirmedIfNew[T <: Transaction](tx: T): Either[ValidationError, T]

  def packUnconfirmed(heightOpt: Option[Int] = None): Seq[Transaction]

  def clearFromUnconfirmed(data: Seq[Transaction]): Unit

  def onNewOffchainTransaction[T <: Transaction](transaction: T): Either[ValidationError, T]
}
