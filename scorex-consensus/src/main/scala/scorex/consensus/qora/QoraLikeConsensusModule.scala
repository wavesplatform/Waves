package scorex.consensus.qora

import com.google.common.primitives.{Bytes, Longs}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.block.{Block, BlockField}
import scorex.consensus.{OneGeneratorConsensusModule, ConsensusModule, PoSConsensusModule}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.hash.FastCryptographicHash._
import scorex.transaction._
import scorex.utils.NTP

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Try


class QoraLikeConsensusModule extends PoSConsensusModule[QoraLikeConsensusBlockData] with OneGeneratorConsensusModule {

  import QoraLikeConsensusModule._

  val GeneratingBalanceLength = 8
  override val generatingBalanceDepth: Int = GeneratingBalanceDepth

  private val ReTarget = 10
  private val MinBalance = 1L
  private val MaxBalance = 10000000000L

  private val MinBlockTime = 1.minute.toSeconds
  private val MaxBlockTime = 5.minute.toSeconds

  implicit val consensusModule: ConsensusModule[QoraLikeConsensusBlockData] = this

  def calculateSignature(prevBlock: Block, history: History, account: PrivateKeyAccount): Array[Byte] = {
    val gb = getNextBlockGeneratingBalance(prevBlock, history)
    val ref = consensusBlockData(prevBlock).generatorSignature
    calculateSignature(ref, gb, account)
  }

  def calculateSignature(reference: Array[Byte],
                         generatingBalance: Long,
                         account: PrivateKeyAccount): Array[Byte] = {
    val generatorSignature = reference.take(GeneratorSignatureLength)

    val genBalanceBytes = Longs.toByteArray(generatingBalance)
      .ensuring(_.size == GeneratingBalanceLength)

    val si = Bytes.concat(generatorSignature, genBalanceBytes, account.publicKey)
    EllipticCurveImpl.sign(account, si)
  }

  def getBaseTarget(generatingBalance: Long): BigInt = BigInt(minMaxBalance(generatingBalance)) * getBlockTime(generatingBalance)

  def getBlockTime(generatingBalance: Long): Long = {
    val percentageOfTotal = minMaxBalance(generatingBalance) / MaxBalance.toDouble
    (MinBlockTime + ((MaxBlockTime - MinBlockTime) * (1 - percentageOfTotal))).toLong
  }

  private def minMaxBalance(generatingBalance: Long) =
    if (generatingBalance < MinBalance) MinBalance
    else if (generatingBalance > MaxBalance) MaxBalance
    else generatingBalance

  private def blockGeneratingBalance(block: Block) = consensusBlockData(block).generatingBalance

  def getNextBlockGeneratingBalance(block: Block, history: History): Long = {
    if (history.heightOf(block).get % ReTarget == 0) {
      //GET FIRST BLOCK OF TARGET
      val firstBlock = (1 until ReTarget).foldLeft(block) { case (bl, _) =>
        history.parent(bl).get
      }

      //CALCULATE THE GENERATING TIME FOR LAST 10 BLOCKS
      val generatingTime = block.timestampField.value - firstBlock.timestampField.value

      //CALCULATE EXPECTED FORGING TIME
      val expectedGeneratingTime = getBlockTime(blockGeneratingBalance(block)) * ReTarget * 1000

      //CALCULATE MULTIPLIER
      val multiplier = expectedGeneratingTime / generatingTime.toDouble

      //CALCULATE NEW GENERATING BALANCE
      val generatingBalance = (blockGeneratingBalance(block) * multiplier).toLong
      minMaxBalance(generatingBalance)
    } else blockGeneratingBalance(block)
  }

  def getNextBlockGeneratingBalance(history: History): Long = {
    val lastBlock = history.lastBlock
    getNextBlockGeneratingBalance(lastBlock, history)
  }

  override def generators(block: Block): Seq[Account] = Seq(block.signerDataField.value.generator)

  override def generateNextBlock[TT](account: PrivateKeyAccount)
                                    (implicit transactionModule: TransactionModule[TT]): Future[Option[Block]] = {
    val version = 1: Byte

    val history = transactionModule.blockStorage.history
    val state = transactionModule.blockStorage.state

    require(state.isInstanceOf[State with BalanceSheet])
    val genBalance = generatingBalance(account)
    require(genBalance > 0, "Zero generating balance in generateNextBlock")
    require(history.isInstanceOf[BlockChain])

    val lastBlock = history.lastBlock

    val signature = calculateSignature(lastBlock, history, account)
    val h = hash(signature)
    val hashValue = BigInt(1, h)

    //CALCULATE ACCOUNT TARGET
    val targetBytes = Array.fill(32)(Byte.MaxValue)
    val baseTarget = getBaseTarget(getNextBlockGeneratingBalance(lastBlock, history))
    //MULTIPLY TARGET BY USER BALANCE
    val target = BigInt(1, targetBytes) / baseTarget * BigInt(genBalance)

    //CALCULATE GUESSES
    val guesses = hashValue / target + 1

    //CALCULATE TIMESTAMP
    val timestampRaw = guesses * 1000 + lastBlock.timestampField.value

    //CHECK IF NOT HIGHER THAN MAX LONG VALUE
    val timestamp = if (timestampRaw > Long.MaxValue) Long.MaxValue else timestampRaw.longValue()

    if (timestamp <= NTP.correctedTime()) {

      val consensusData = new QoraLikeConsensusBlockData {
        override val generatorSignature: Array[Byte] = signature
        override val generatingBalance: Long = getNextBlockGeneratingBalance(lastBlock, history)
      }
      Future(Some(Block.buildAndSign(version,
        timestamp,
        lastBlock.uniqueId,
        consensusData,
        transactionModule.packUnconfirmed(),
        account)))
    } else Future(None)
  }

  override def parseBytes(bytes: Array[Byte]): Try[BlockField[QoraLikeConsensusBlockData]] = Try {
    QoraConsensusBlockField(new QoraLikeConsensusBlockData {
      override val generatingBalance: Long = Longs.fromByteArray(bytes.take(GeneratingBalanceLength))
      override val generatorSignature: Array[Byte] = bytes.takeRight(GeneratorSignatureLength)
    })
  }

  override def isValid[TT](block: Block)(implicit transactionModule: TransactionModule[TT]): Boolean = {
    val history = transactionModule.blockStorage.history
    val state = transactionModule.blockStorage.state

    val data = consensusBlockData(block)

    if (data.generatingBalance != getNextBlockGeneratingBalance(history.parent(block).get, history)) {
      //CHECK IF GENERATING BALANCE IS CORRECT
      false
    } else {
      //target base
      val targetBytes = Array.fill(32)(Byte.MaxValue)
      val baseTarget: BigInt = getBaseTarget(data.generatingBalance)
      val gen = block.signerDataField.value.generator
      val genBalance = BigInt(generatingBalance(gen))
      val target0 = BigInt(1, targetBytes) / baseTarget * genBalance

      //target bounds
      val guesses = (block.timestampField.value - history.parent(block).get.timestampField.value) / 1000
      val lowerTarget = target0 * (guesses - 1)
      val target = target0 * guesses

      val hit = BigInt(1, hash(data.generatorSignature))

      //generation check
      hit >= lowerTarget && hit < target
    }
  }

  override def genesisData: BlockField[QoraLikeConsensusBlockData] =
    QoraConsensusBlockField(new QoraLikeConsensusBlockData {
      override val generatingBalance: Long = 10000000
      override val generatorSignature: Array[Byte] = Array.fill(64)(0: Byte)
    })

  override def formBlockData(data: QoraLikeConsensusBlockData): BlockField[QoraLikeConsensusBlockData] =
    QoraConsensusBlockField(data)

  override def blockScore(block: Block)(implicit transactionModule: TransactionModule[_]): BigInt = BigInt(1)

  override def consensusBlockData(block: Block): QoraLikeConsensusBlockData = block.consensusDataField.value match {
    case b: QoraLikeConsensusBlockData => b
    case m => throw new AssertionError(s"Only QoraLikeConsensusBlockData is available, $m given")
  }

}

object QoraLikeConsensusModule {
  val GeneratorSignatureLength = 64
  val GeneratingBalanceDepth = 50
}
