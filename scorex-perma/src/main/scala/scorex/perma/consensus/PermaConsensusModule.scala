package scorex.perma.consensus

import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.block.{Block, BlockField}
import scorex.consensus.ConsensusModule
import scorex.crypto.SigningFunctions._
import scorex.crypto.{CryptographicHash, EllipticCurveImpl, Sha256}
import scorex.perma.settings.Constants
import scorex.transaction.TransactionModule

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

/**
  * Data and functions related to a consensus algo
  */

class PermaConsensusModule extends ConsensusModule[PermaLikeConsensusBlockData] {

  val MiningReward = 1000000
  val InitialDifficulty = BigInt(Array.fill(32)(1: Byte)).toLong
  val GenesisCreator = new PublicKeyAccount(Array())
  val RootHash: Array[Byte] = "".getBytes

  private def blockGenerator(block: Block) = block.signerDataField.value.generator

  def isValid[TT](block: Block)(implicit transactionModule: TransactionModule[TT]): Boolean = {
    val f = block.consensusDataField.asInstanceOf[PermaConsensusBlockField]
    val publicKey = blockGenerator(block).publicKey
    validate(publicKey, f.value.puz, f.value.difficulty, f.value.ticket, RootHash)
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

  def blockScore(block: Block)(implicit transactionModule: TransactionModule[_]): BigInt = {
    ticketScore(block.consensusDataField.asInstanceOf[PermaConsensusBlockField].value.ticket)
  }

  def generateNextBlock[TT](account: PrivateKeyAccount)
                           (implicit transactionModule: TransactionModule[TT]): Future[Option[Block]] = {
    //TODO
    Future(None)
  }

  override def consensusBlockData(block: Block): PermaLikeConsensusBlockData =
    block.consensusDataField.value.asInstanceOf[PermaLikeConsensusBlockData]

  //TODO return Try
  override def parseBlockData(bytes: Array[Byte]): PermaConsensusBlockField = PermaConsensusBlockField.parse(bytes)

  override def genesisData: PermaConsensusBlockField =
    PermaConsensusBlockField(PermaLikeConsensusBlockData(
      InitialDifficulty,
      Array[Byte](),
      Ticket(GenesisCreator.publicKey, Array(), IndexedSeq())
    ))

  override def formBlockData(data: PermaLikeConsensusBlockData): BlockField[PermaLikeConsensusBlockData] =
    PermaConsensusBlockField(data)

  private val NoSig = Array[Byte]()

  //todo: validate r\i
  private def validate(publicKey: PublicKey,
                       puz: Array[Byte],
                       difficulty: BigInt,
                       t: Ticket,
                       rootHash: CryptographicHash.Digest): Boolean = Try {
    val proofs = t.proofs
    require(proofs.size == Constants.k)

    //Local-POR lottery verification

    val sigs = NoSig +: proofs.map(_.signature)
    val ris = proofs.map(_.segmentIndex)

    val partialProofsCheck = 1.to(Constants.k).foldLeft(true) { case (partialResult, i) =>
      val segment = proofs(i - 1).segment

      segment.check(ris(i - 1), rootHash)() || {
        val hi = Sha256.hash(puz ++ publicKey ++ sigs(i - 1) ++ segment.data)
        EllipticCurveImpl.verify(sigs(i), hi, publicKey)
      }
    }
    partialProofsCheck && (ticketScore(t) < difficulty)
  }.getOrElse(false)

  private def ticketScore(t: Ticket): BigInt = BigInt(1, Sha256.hash(t.proofs.map(_.signature).reduce(_ ++ _)))

}