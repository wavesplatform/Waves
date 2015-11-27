package scorex.perma.consensus

import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.block.{Block, BlockField}
import scorex.consensus.ConsensusModule
import scorex.crypto.SigningFunctions._
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.CryptographicHash.Digest
import scorex.perma.settings.Constants
import scorex.perma.settings.Constants._
import scorex.storage.Storage
import scorex.transaction.{BalanceSheet, BlockChain, TransactionModule}
import scorex.utils._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

/**
  * Data and functions related to a consensus algo
  */
class PermaConsensusModule(rootHash: Array[Byte])
                          (implicit val authDataStorage: Storage[Long, AuthDataBlock[DataSegment]])
  extends ConsensusModule[PermaLikeConsensusBlockData] with ScorexLogging {

  val InitialDifficulty:BigInt = BigInt(Array.fill(36)(1: Byte))
  val GenesisCreator = new PublicKeyAccount(Array())
  val Version: Byte = 1
  val hash = Constants.hash

  implicit val consensusModule: ConsensusModule[PermaLikeConsensusBlockData] = this

  def miningReward(block: Block) = 1000000

  private def blockGenerator(block: Block) = block.signerDataField.value.generator

  def isValid[TT](block: Block)(implicit transactionModule: TransactionModule[TT]): Boolean = {
    val f = block.consensusDataField.asInstanceOf[PermaConsensusBlockField]
    val publicKey = blockGenerator(block).publicKey
    validate(publicKey, f.value.puz, f.value.difficulty, f.value.ticket, rootHash)
    //TODO check puz and previous block
  }

  /**
    * Fees could go to a single miner(forger) usually, but can go to many parties, e.g. see
    * Meni Rosenfeld's Proof-of-Activity proposal http://eprint.iacr.org/2014/452.pdf
    */
  def feesDistribution(block: Block): Map[Account, Long] =
    Map(blockGenerator(block) -> (miningReward(block) + block.transactions.map(_.fee).sum))

  /**
    * Get block producers(miners/forgers). Usually one miner produces a block, but in some proposals not
    * (see e.g. Meni Rosenfeld's Proof-of-Activity paper http://eprint.iacr.org/2014/452.pdf)
    */
  def generators(block: Block): Seq[Account] = Seq(blockGenerator(block))

  def blockScore(block: Block)(implicit transactionModule: TransactionModule[_]): BigInt = BigInt(1)

  def generateNextBlock[TT](account: PrivateKeyAccount)
                           (implicit transactionModule: TransactionModule[TT]): Future[Option[Block]] = Try {

    val lastBlock = transactionModule.history.asInstanceOf[BlockChain].lastBlock
    val lastBlockKernelData = lastBlock.consensusDataField.asInstanceOf[PermaConsensusBlockField].value
    val lastBlockTime = lastBlock.timestampField.value

    val eta = (NTP.correctedTime() - lastBlockTime) / 1000
    val puz = generatePuz(lastBlock)

    log.debug(s"eta $eta, " +
      s"account:  $account " +
      s"account balance: ${transactionModule.state.asInstanceOf[BalanceSheet].generationBalance(account)}"
    )

    val keyPair = (account.privateKey, account.publicKey)
    val ticket = generate(keyPair, puz)
    val difficulty = calcDifficulty(lastBlock)

    if (validate(keyPair._2, puz, difficulty, ticket, rootHash)) {
      val timestamp = NTP.correctedTime()
      val consensusData = PermaLikeConsensusBlockData(difficulty, puz, ticket)

      Future(Some(Block.buildAndSign(Version,
        timestamp,
        lastBlock.uniqueId,
        consensusData,
        transactionModule.packUnconfirmed(),
        account)))

    } else {
      Future(None)
    }
  }.recoverWith { case t: Throwable =>
    log.error("Error when creating new block", t)
    t.printStackTrace()
    Try(Future(None))
  }.getOrElse(Future(None))

  override def consensusBlockData(block: Block): PermaLikeConsensusBlockData =
    block.consensusDataField.value.asInstanceOf[PermaLikeConsensusBlockData]

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
                       rootHash: Digest): Boolean = Try {
    val proofs = t.proofs
    require(proofs.size == Constants.k)

    //Local-POR lottery verification

    val sigs = NoSig +: proofs.map(_.signature)
    val ris = proofs.map(_.segmentIndex)

    val partialProofsCheck = 1.to(Constants.k).foldLeft(true) { case (partialResult, i) =>
      val segment = proofs(i - 1).segment

      segment.check(ris(i - 1), rootHash)() || {
        val hi = hash.hash(puz ++ publicKey ++ sigs(i - 1) ++ segment.data)
        EllipticCurveImpl.verify(sigs(i), hi, publicKey)
      }
    }
    partialProofsCheck && (ticketScore(t) < difficulty)
  }.getOrElse(false)

  private def generatePuz(block: Block) = hash.hash(block.bytes)

  private def ticketScore(t: Ticket): BigInt = if(t.proofs.nonEmpty) {
    BigInt(1, hash.hash(t.proofs.map(_.signature).reduce(_ ++ _)))
  } else {
    //Genesis block contains empty ticket
    0
  }

  private def generate(keyPair: (PrivateKey, PublicKey), puz: Array[Byte]): Ticket = {

    val (privateKey, publicKey) = keyPair

    //scratch-off for the Local-POR lottery
    val s = randomBytes(32)

    val sig0 = NoSig
    val r1 = u(publicKey, (BigInt(1, hash.hash(puz ++ publicKey ++ s)) % Constants.l).toInt)

    val proofs: IndexedSeq[PartialProof] = 1.to(Constants.k).foldLeft(
      (r1, sig0, Seq[PartialProof]())
    ) {
      case ((ri, sig_prev, seq), _) =>
        val segment = authDataStorage.get(ri).get
        val hi = hash.hash(puz ++ publicKey ++ sig_prev ++ segment.data)
        val sig = EllipticCurveImpl.sign(privateKey, hi)
        val r_next = u(publicKey, BigInt(1, hash.hash(puz ++ publicKey ++ sig)).mod(Constants.l).toInt)

        (r_next, sig, seq :+ PartialProof(sig, ri, segment))
    }._3.toIndexedSeq.ensuring(_.size == Constants.k)

    Ticket(publicKey, s, proofs)
  }

  //calculate index of i-th segment
  private def u(pubKey: PublicKey, i: Int): Long = {
    val h = hash.hash(pubKey ++ BigInt(i).toByteArray)
    BigInt(1, h).mod(Constants.n).toLong
  }

  //TODO implement
  private def calcDifficulty(lastBlock: Block): BigInt = {
    InitialDifficulty
  }


}