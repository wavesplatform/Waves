package scorex.consensus

import scorex.account.{PrivateKeyAccount, Account}
import scorex.block.{BlockProcessingModule, Block}
import scorex.transaction.{TransactionModule, History, State}

import scala.concurrent.Future


trait ConsensusModule[ConsensusBlockData] extends BlockProcessingModule[ConsensusBlockData]{

  def isValid[TT](block: Block)(implicit transactionModule: TransactionModule[TT]): Boolean

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

  def generateNextBlock[TT](account: PrivateKeyAccount)
                           (implicit transactionModule: TransactionModule[TT]): Future[Option[Block]]

  def consensusBlockData(block: Block): ConsensusBlockData
}
