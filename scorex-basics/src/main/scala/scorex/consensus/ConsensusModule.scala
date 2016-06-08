package scorex.consensus

import scorex.account.{Account, PrivateKeyAccount}
import scorex.block.{Block, BlockProcessingModule}
import scorex.transaction.{BalanceSheet, TransactionModule}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


trait ConsensusModule[ConsensusBlockData] extends BlockProcessingModule[ConsensusBlockData] {

  def isValid[TransactionalBlockData](block: Block)(implicit transactionModule: TransactionModule[TransactionalBlockData]): Boolean

  /**
    * Fees could go to a single miner(forger) usually, but can go to many parties, e.g. see
    * Meni Rosenfeld's Proof-of-Activity proposal http://eprint.iacr.org/2014/452.pdf
    */
  def feesDistribution(block: Block): Map[Account, Long]

  /**
    * Get block producers(miners/forgers). Usually one miner produces a block, but in some proposals not
    * (see e.g. Meni Rosenfeld's Proof-of-Activity paper http://eprint.iacr.org/2014/452.pdf)
    * @param block
    * @return
    */
  def generators(block: Block): Seq[Account]

  def blockScore(block: Block)(implicit transactionModule: TransactionModule[_]): BigInt

  def generateNextBlock[TransactionalBlockData](account: PrivateKeyAccount)
                           (implicit transactionModule: TransactionModule[TransactionalBlockData]): Future[Option[Block]]

  def generateNextBlocks[TransactionalBlockData](accounts: Seq[PrivateKeyAccount])
                           (implicit transactionModule: TransactionModule[TransactionalBlockData]): Future[Seq[Block]] = {
    Future.sequence(accounts.map(acc => generateNextBlock(acc))).map(_.flatten)
  }

  def generatingBalance[TransactionalBlockData](address: String)
                           (implicit transactionModule: TransactionModule[TransactionalBlockData]): Long = {
    transactionModule.blockStorage.state.asInstanceOf[BalanceSheet]
      .balanceWithConfirmations(address, generatingBalanceDepth)
  }

  def generatingBalance[TransactionalBlockData](account: Account)
                           (implicit transactionModule: TransactionModule[TransactionalBlockData]): Long =
    generatingBalance(account.address)

  def generatingBalanceDepth: Int

  def consensusBlockData(block: Block): ConsensusBlockData
}
