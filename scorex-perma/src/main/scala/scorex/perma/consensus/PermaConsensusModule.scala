package scorex.perma.consensus

import scala.concurrent.ExecutionContext.Implicits.global
import scorex.account.{PrivateKeyAccount, Account}
import scorex.block.{BlockField, Block}
import scorex.consensus.ConsensusModule
import scorex.transaction.TransactionModule

import scala.concurrent.Future

/**
 * Data and functions related to a consensus algo
 */

class PermaConsensusModule extends ConsensusModule[PermaLikeConsensusBlockData] {

  def isValid[TT](block: Block)(implicit transactionModule: TransactionModule[TT]): Boolean = {
    //TODO
    false
  }

  /**
    * Fees could go to a single miner(forger) usually, but can go to many parties, e.g. see
    * Meni Rosenfeld's Proof-of-Activity proposal http://eprint.iacr.org/2014/452.pdf
    */
  def feesDistribution(block: Block): Map[Account, Long] = {
    //TODO
    Map()
  }

  /**
    * Get block producers(miners/forgers). Usually one miner produces a block, but in some proposals not
    * (see e.g. Meni Rosenfeld's Proof-of-Activity paper http://eprint.iacr.org/2014/452.pdf)
    * @param block
    * @return
    */
  def generators(block: Block): Seq[Account] = {
    //TODO
    Seq()
  }

  def blockScore(block: Block)(implicit transactionModule: TransactionModule[_]): BigInt =  BigInt(1)

  def generateNextBlock[TT](account: PrivateKeyAccount)
                           (implicit transactionModule: TransactionModule[TT]): Future[Option[Block]] = {
    //TODO
    Future(None)
  }

  override def consensusBlockData(block: Block): PermaLikeConsensusBlockData =
    block.consensusDataField.value.asInstanceOf[PermaLikeConsensusBlockData]

  override def parseBlockData(bytes: Array[Byte]): BlockField[PermaLikeConsensusBlockData] =
    PermaConsensusBlockField(new PermaLikeConsensusBlockData{
      override val generatorSignature: Array[Byte] = bytes
    })

  override def genesisData: BlockField[PermaLikeConsensusBlockData] =
    PermaConsensusBlockField(new PermaLikeConsensusBlockData {
      override val generatorSignature: Array[Byte] = Array.fill(32)(0: Byte)
    })

  override def formBlockData(data: PermaLikeConsensusBlockData): BlockField[PermaLikeConsensusBlockData] =
    PermaConsensusBlockField(data)

}