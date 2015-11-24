package scorex.perma.consensus

import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.block.{Block, BlockField}
import scorex.consensus.ConsensusModule
import scorex.perma.actors.Ticket
import scorex.transaction.TransactionModule

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

/**
  * Data and functions related to a consensus algo
  */

class PermaConsensusModule extends ConsensusModule[PermaLikeConsensusBlockData] {

  val MiningReward = 1000000
  val InitialDifficulty = BigInt(Array.fill(32)(1: Byte))
  val GenesisCreator = new PublicKeyAccount(Array())

  private def blockGenerator(block: Block) = block.signerDataField.value.generator

  def isValid[TT](block: Block)(implicit transactionModule: TransactionModule[TT]): Boolean = {
    //TODO
    false
  }

  /**
    * Fees could go to a single miner(forger) usually, but can go to many parties, e.g. see
    * Meni Rosenfeld's Proof-of-Activity proposal http://eprint.iacr.org/2014/452.pdf
    */
  def feesDistribution(block: Block): Map[Account, Long] =
    Map(blockGenerator(block) -> MiningReward)

  /**
    * Get block producers(miners/forgers). Usually one miner produces a block, but in some proposals not
    * (see e.g. Meni Rosenfeld's Proof-of-Activity paper http://eprint.iacr.org/2014/452.pdf)
    * @param block
    * @return
    */
  def generators(block: Block): Seq[Account] = Seq(blockGenerator(block))

  def blockScore(block: Block)(implicit transactionModule: TransactionModule[_]): BigInt = BigInt(1)

  def generateNextBlock[TT](account: PrivateKeyAccount)
                           (implicit transactionModule: TransactionModule[TT]): Future[Option[Block]] = {
    //TODO
    Future(None)
  }

  override def consensusBlockData(block: Block): PermaLikeConsensusBlockData =
    block.consensusDataField.value.asInstanceOf[PermaLikeConsensusBlockData]

  override def parseBlockData(bytes: Array[Byte]): Try[BlockField[PermaLikeConsensusBlockData]] = ???

  /*
    PermaConsensusBlockField(new PermaLikeConsensusBlockData{
      ???
    })*/

  override def genesisData: BlockField[PermaLikeConsensusBlockData] =
    PermaConsensusBlockField(new PermaLikeConsensusBlockData {
      override val difficulty = InitialDifficulty
      override val puz = Array[Byte]()
      override val ticket = Ticket(GenesisCreator.publicKey, Array(), IndexedSeq())
    })

  /*
    PermaConsensusBlockField(new PermaLikeConsensusBlockData {
      override val generatorSignature: Array[Byte] = Array.fill(32)(0: Byte)
    })*/

  override def formBlockData(data: PermaLikeConsensusBlockData): BlockField[PermaLikeConsensusBlockData] =
    PermaConsensusBlockField(data)
}