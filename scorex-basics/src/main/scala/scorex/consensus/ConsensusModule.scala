package scorex.consensus

import scorex.account.{Account, PrivateKeyAccount}
import scorex.block.{Block, BlockProcessingModule}
import scorex.transaction.TransactionModule

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


trait ConsensusModule[ConsensusBlockData] extends BlockProcessingModule[ConsensusBlockData] {

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

  def generateNextBlocks[T](accounts: Seq[PrivateKeyAccount])
                           (implicit transactionModule: TransactionModule[T]): Future[Seq[Block]] = {
    Future.sequence(accounts.map(acc => generateNextBlock(acc))).map(_.flatten)
  }

  def consensusBlockData(block: Block): ConsensusBlockData
}
