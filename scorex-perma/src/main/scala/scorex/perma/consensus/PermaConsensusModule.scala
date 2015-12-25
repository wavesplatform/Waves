package scorex.perma.consensus

import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.block.{Block, BlockField}
import scorex.consensus.ConsensusModule
import scorex.crypto.CryptographicHash.Digest
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.crypto.singing.SigningFunctions.{PrivateKey, PublicKey}
import scorex.perma.settings.Constants
import scorex.perma.settings.Constants._
import scorex.storage.Storage
import scorex.transaction.TransactionModule
import scorex.utils.{NTP, ScorexLogging, randomBytes}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Try}

/**
  * Data and functions related to a consensus algo
  */
class PermaConsensusModule(rootHash: Array[Byte])
                          (implicit val authDataStorage: Storage[Long, AuthDataBlock[DataSegment]])
  extends ConsensusModule[PermaConsensusBlockData] with ScorexLogging {

  val InitialTarget = Constants.initialTarget
  val initialTargetPow: BigInt = log2(InitialTarget)
  val TargetRecalculation = Constants.targetRecalculation
  val AvgDelay = Constants.averageDelay
  val Hash = Constants.hash
  val SSize = Hash.DigestSize
  require(SSize == PermaConsensusBlockField.SLength)

  val GenesisCreator = new PublicKeyAccount(Array.fill(PermaConsensusBlockField.PublicKeyLength)(0: Byte))
  val Version: Byte = 1

  implicit val consensusModule: ConsensusModule[PermaConsensusBlockData] = this

  def miningReward(block: Block) = if (blockGenerator(block).publicKey sameElements GenesisCreator.publicKey) 0
  else 1000000

  private def blockGenerator(block: Block) = block.signerDataField.value.generator

  def isValid[TT](block: Block)(implicit transactionModule: TransactionModule[TT]): Boolean = {
    val f = consensusBlockData(block)
    val trans = transactionModule.blockStorage.history
    trans.parent(block) match {
      case Some(parent) =>
        lazy val publicKey = blockGenerator(block).publicKey
        lazy val puzIsValid = f.puz sameElements generatePuz(parent)
        lazy val targetIsValid = f.target == calcTarget(parent)
        lazy val ticketIsValid = validate(publicKey, f.puz, f.target, f.ticket, rootHash)
        if (puzIsValid && targetIsValid && ticketIsValid)
          true
        else {
          log.debug(
            s"Non-valid block: puzIsValid=$puzIsValid, targetIsValid=$targetIsValid && ticketIsValid=$ticketIsValid"
          )
          false
        }

      case None =>
        true
    }
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

  def blockScore(block: Block)(implicit transactionModule: TransactionModule[_]): BigInt = {
    val score = initialTargetPow -
      log2(consensusBlockData(block).target)
    if (score > 0) score else 1
  }

  def generateNextBlock[TT](account: PrivateKeyAccount)
                           (implicit transactionModule: TransactionModule[TT]): Future[Option[Block]] = Try {

    val parent = transactionModule.blockStorage.history.lastBlock
    val puz = generatePuz(parent)

    val keyPair = (account.privateKey, account.publicKey)
    val ticket = generate(keyPair, puz)
    val target = calcTarget(parent)

    if (validate(keyPair._2, puz, target, ticket, rootHash)) {
      val timestamp = NTP.correctedTime()
      val consensusData = PermaConsensusBlockData(target, puz, ticket)

      Future(Some(Block.buildAndSign(Version,
        timestamp,
        parent.uniqueId,
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

  override def consensusBlockData(block: Block): PermaConsensusBlockData = block.consensusDataField.value match {
    case b: PermaConsensusBlockData => b
    case m => throw new AssertionError(s"Only PermaLikeConsensusBlockData is available, $m given")
  }

  override def parseBlockData(bytes: Array[Byte]): Try[PermaConsensusBlockField] =
    PermaConsensusBlockField.parse(bytes)

  override def genesisData: PermaConsensusBlockField =
    PermaConsensusBlockField(PermaConsensusBlockData(
      InitialTarget,
      Array.fill(PermaConsensusBlockField.PuzLength)(0: Byte),
      Ticket(GenesisCreator.publicKey, Array.fill(PermaConsensusBlockField.SLength)(0: Byte), IndexedSeq())
    ))

  override def formBlockData(data: PermaConsensusBlockData): BlockField[PermaConsensusBlockData] =
    PermaConsensusBlockField(data)

  def generatePuz(block: Block) = Hash.hash(block.bytes)

  private val NoSig = Array[Byte]()

  private[consensus] def validate(publicKey: PublicKey,
                                  puz: Array[Byte],
                                  target: BigInt,
                                  t: Ticket,
                                  rootHash: Digest): Boolean = Try {
    val proofs = t.proofs
    require(proofs.size == Constants.k)
    require(t.s.length == SSize)

    val sigs = NoSig +: proofs.map(_.signature)
    val ris = proofs.map(_.segmentIndex)
    require(ris(0) == calculateIndex(publicKey, (BigInt(1, Hash.hash(puz ++ publicKey ++ t.s)) % Constants.l).toInt))

    val partialProofsCheck = 1.to(Constants.k).foldLeft(true) { case (partialResult, i) =>
      val segment = proofs(i - 1).segment
      val rc = calculateIndex(publicKey,
        BigInt(1, Hash.hash(puz ++ publicKey ++ proofs(i - 1).signature)).mod(Constants.l).toInt)

      segment.check(ris(i - 1), rootHash)() && {
        val hi = Hash.hash(puz ++ publicKey ++ sigs(i - 1) ++ segment.data)
        EllipticCurveImpl.verify(sigs(i), hi, publicKey)
      } && (ris.length == i || rc == ris(i))
    }
    partialProofsCheck && (ticketScore(t) < target)
  }.getOrElse(false)

  private[consensus] def ticketScore(t: Ticket): BigInt = if (t.proofs.nonEmpty) {
    BigInt(1, Hash.hash(t.proofs.map(_.signature).reduce(_ ++ _)))
  } else 0

  private[consensus] def generate(keyPair: (PrivateKey, PublicKey), puz: Array[Byte]): Ticket = {

    val (privateKey, publicKey) = keyPair

    //scratch-off for the Local-POR lottery
    val s = randomBytes(SSize)

    val sig0 = NoSig
    val r1 = calculateIndex(publicKey, (BigInt(1, Hash.hash(puz ++ publicKey ++ s)) % Constants.l).toInt)

    val proofs: IndexedSeq[PartialProof] = 1.to(Constants.k).foldLeft(
      (r1, sig0, Seq[PartialProof]())
    ) {
      case ((ri, sig_prev, seq), _) =>
        val segment = authDataStorage.get(ri).get
        val hi = Hash.hash(puz ++ publicKey ++ sig_prev ++ segment.data)
        val sig = EllipticCurveImpl.sign(privateKey, hi)
        val rNext = calculateIndex(publicKey, BigInt(1, Hash.hash(puz ++ publicKey ++ sig)).mod(Constants.l).toInt)

        (rNext, sig, seq :+ PartialProof(sig, ri, segment))
    }._3.toIndexedSeq.ensuring(_.size == Constants.k)

    Ticket(publicKey, s, proofs)
  }

  //calculate index of i-th segment
  private[consensus] def calculateIndex(pubKey: PublicKey, i: Int): Long = {
    val h = Hash.hash(pubKey ++ BigInt(i).toByteArray)
    BigInt(1, h).mod(Constants.n).toLong
  }

  private def calcTarget(block: Block)(implicit transactionModule: TransactionModule[_]): BigInt = {
    val trans = transactionModule.blockStorage.history
    val currentTarget = consensusBlockData(block).target
    Try {
      val height = trans.heightOf(block).get
      if (height % TargetRecalculation == 0 && height > TargetRecalculation) {
        val lastAvgDuration: BigInt = trans.averageDelay(block, TargetRecalculation).get
        val newTarget = currentTarget * lastAvgDuration / 1000 / AvgDelay
        log.debug(s"Height: $height, target:$newTarget vs $currentTarget, lastAvgDuration:$lastAvgDuration")
        newTarget
      } else {
        currentTarget
      }
    }.recoverWith { case t: Throwable =>
      log.error(s"Error when calculating target: ${t.getMessage}")
      t.printStackTrace()
      Success(currentTarget)
    }.getOrElse(currentTarget)
  }

  private def log2(i: BigInt): BigInt = BigDecimal(math.log(i.doubleValue()) / math.log(2)).toBigInt()
}