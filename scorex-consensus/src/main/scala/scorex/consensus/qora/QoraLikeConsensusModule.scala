package scorex.consensus.qora

import com.google.common.primitives.{Bytes, Longs}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.block.{Block, BlockField}
import scorex.consensus.{LagonakiConsensusModule, ConsensusModule}
import scorex.crypto.Sha256._
import scorex.crypto.SigningFunctionsImpl
import scorex.transaction._
import scorex.utils.NTP


class QoraLikeConsensusModule extends LagonakiConsensusModule[QoraLikeConsensusBlockData] {
  import QoraLikeConsensusModule.GeneratorSignatureLength

  val GeneratingBalanceLength = 8

  private val RETARGET = 10
  private val MIN_BALANCE = 1L
  private val MAX_BALANCE = 10000000000L
  private val MIN_BLOCK_TIME = 1 * 60
  private val MAX_BLOCK_TIME = 5 * 60

  implicit val consensusModule: ConsensusModule[QoraLikeConsensusBlockData] = this

  //todo: asInstanceOf ?
  private def consensusBlockData(block: Block): QoraLikeConsensusBlockData =
    block.consensusDataField.value.asInstanceOf[QoraLikeConsensusBlockData]


  override def generators(block: Block): Seq[Account] = Seq(block.signerDataField.value.generator)

  override def generateNextBlock[TT](account: PrivateKeyAccount,
                                     state: State,
                                     history: History)
                                    (implicit transactionModule: TransactionModule[TT]): Option[Block] = {
    val version = 1: Byte

    require(state.isInstanceOf[State with BalanceSheet])
    val generationBalance = state.asInstanceOf[State with BalanceSheet].generationBalance(account)
    require(generationBalance > 0, "Zero generating balance in generateNextBlock")
    require(history.isInstanceOf[BlockChain])

    val lastBlock = history.asInstanceOf[BlockChain].lastBlock

    val signature = calculateSignature(lastBlock, history, account)
    val h = hash(signature)
    val hashValue = BigInt(1, h)

    //CALCULATE ACCOUNT TARGET
    val targetBytes = Array.fill(32)(Byte.MaxValue)
    val baseTarget = getBaseTarget(getNextBlockGeneratingBalance(lastBlock, history))
    //MULTIPLY TARGET BY USER BALANCE
    val target = BigInt(1, targetBytes) / baseTarget * BigInt(generationBalance)

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
      Some(Block.buildAndSign(version,
        timestamp,
        lastBlock.uniqueId,
        consensusData,
        transactionModule.packUnconfirmed(),
        account))
    } else None
  }

  override def blockScore(block: Block, history: History)
                         (implicit transactionModule: TransactionModule[_]): BigInt = BigInt(1)

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
    SigningFunctionsImpl.sign(account, si)
  }

  def getBaseTarget(generatingBalance: Long): BigInt = BigInt(minMaxBalance(generatingBalance)) * getBlockTime(generatingBalance)

  def getBlockTime(generatingBalance: Long): Long = {
    val percentageOfTotal = minMaxBalance(generatingBalance) / MAX_BALANCE.toDouble
    (MIN_BLOCK_TIME + ((MAX_BLOCK_TIME - MIN_BLOCK_TIME) * (1 - percentageOfTotal))).toLong
  }

  private def minMaxBalance(generatingBalance: Long) =
    if (generatingBalance < MIN_BALANCE) MIN_BALANCE
    else if (generatingBalance > MAX_BALANCE) MAX_BALANCE
    else generatingBalance

  private def blockGeneratingBalance(block: Block) = consensusBlockData(block).generatingBalance

  def getNextBlockGeneratingBalance(block: Block, history: History): Long = {
    if (history.heightOf(block).get % RETARGET == 0) {
      //GET FIRST BLOCK OF TARGET
      val firstBlock = (1 to RETARGET - 1).foldLeft(block) { case (bl, _) =>
        history.parent(bl).get
      }

      //CALCULATE THE GENERATING TIME FOR LAST 10 BLOCKS
      val generatingTime = block.timestampField.value - firstBlock.timestampField.value

      //CALCULATE EXPECTED FORGING TIME
      val expectedGeneratingTime = getBlockTime(blockGeneratingBalance(block)) * RETARGET * 1000

      //CALCULATE MULTIPLIER
      val multiplier = expectedGeneratingTime / generatingTime.toDouble

      //CALCULATE NEW GENERATING BALANCE
      val generatingBalance = (blockGeneratingBalance(block) * multiplier).toLong
      minMaxBalance(generatingBalance)
    } else blockGeneratingBalance(block)
  }

  override def parseBlockData(bytes: Array[Byte]): BlockField[QoraLikeConsensusBlockData] =
    QoraConsensusBlockField(new QoraLikeConsensusBlockData {
      override val generatingBalance: Long = Longs.fromByteArray(bytes.take(GeneratingBalanceLength))
      override val generatorSignature: Array[Byte] = bytes.takeRight(GeneratorSignatureLength)
    })

  override def isValid[TT](block: Block, history: History, state: State)
                          (implicit transactionModule: TransactionModule[TT]): Boolean = {
    val data = block.consensusDataField.asInstanceOf[QoraConsensusBlockField].value

    if (data.generatingBalance != getNextBlockGeneratingBalance(history.parent(block).get, history)) {
      //CHECK IF GENERATING BALANCE IS CORRECT
      false
    } else {
      //target base
      val targetBytes = Array.fill(32)(Byte.MaxValue)
      val baseTarget:BigInt = getBaseTarget(data.generatingBalance)
      val gen = block.signerDataField.value.generator.address
      val genBalance = BigInt(state.asInstanceOf[BalanceSheet].generationBalance(gen))
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

  def formBlockData(data: QoraLikeConsensusBlockData): BlockField[QoraLikeConsensusBlockData] =
    QoraConsensusBlockField(data)
}


object QoraLikeConsensusModule {
  val GeneratorSignatureLength = 64
}