package scorex.transaction

import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData

trait TransactionModule {

  def blockStorage: BlockStorage

  def validate[T <: Transaction](tx: T): Either[ValidationError, T]

  def unconfirmedTxs: Seq[Transaction]

  def putUnconfirmedIfNew[T <: Transaction](tx: T): Either[ValidationError, T]

  def packUnconfirmed(heightOpt: Option[Int] = None): Seq[Transaction]

  def clearFromUnconfirmed(data: Seq[Transaction]): Unit

  def onNewOffchainTransaction[T <: Transaction](transaction: T): Either[ValidationError, T]

  def generatingBalance(account: Account, atHeight: Int): Long

  def generateNextBlock(account: PrivateKeyAccount): Option[Block]

  def nextBlockGenerationTime(block: Block, account: PublicKeyAccount): Option[Long]

  def blockOrdering: Ordering[(Block)]

  def isValid(block: Block): Boolean
}
object TransactionModule {
  implicit class TransactionModuleExt(t:TransactionModule) {
    def generateNextBlocks(accounts: Seq[PrivateKeyAccount]): Seq[Block] =
      accounts.flatMap(t.generateNextBlock)
  }
}
