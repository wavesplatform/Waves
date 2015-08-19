package scorex.consensus.qora

import com.google.common.primitives.{Bytes, Longs}
import scorex.app.Controller
import scorex.app.utils.NTP
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, BlockStub}
import scorex.consensus.BlockGenerationFunctions
import scorex.crypto.SigningFunctionsImpl
import scorex.app.settings.Constants
import scorex.crypto.HashFunctionsImpl._


//!! a lot of asInstanceOf[QoraBlockGenerationData] in the code, not type-safe
object QoraBlockGenerationFunctions extends BlockGenerationFunctions {
  private val RETARGET = 10
  private val MIN_BALANCE = 1L
  private val MAX_BALANCE = 10000000000L
  private val MIN_BLOCK_TIME = 1 * 60
  private val MAX_BLOCK_TIME = 5 * 60

  override protected def generateNextBlock(account: PrivateKeyAccount, lastBlock: Block): Option[BlockStub] = {
    require(Controller.blockchainStorage.generationBalance(account) > BigDecimal(0), "Zero generating balance in generateNextBlock")

    val signature = calculateSignature(lastBlock, account)
    val h = hash(signature)
    val hashValue = BigInt(1, h)

    //CALCULATE ACCOUNT TARGET
    val targetBytes = Array.fill(32)(Byte.MaxValue)
    val baseTarget = BigInt(getBaseTarget(getNextBlockGeneratingBalance(lastBlock)))
    //MULTIPLY TARGET BY USER BALANCE
    val target = BigInt(1, targetBytes) / baseTarget * Controller.blockchainStorage.generationBalance(account).toBigInt()

    //CALCULATE GUESSES
    val guesses = hashValue / target + 1

    //CALCULATE TIMESTAMP
    val timestampRaw = guesses * 1000 + lastBlock.timestamp

    //CHECK IF NOT HIGHER THAN MAX LONG VALUE
    val timestamp = if (timestampRaw > Long.MaxValue) Long.MaxValue else timestampRaw.longValue()

    if (timestamp <= NTP.correctedTime()) {
      Some(BlockStub(Block.Version, lastBlock.signature, timestamp, account,
        new QoraBlockGenerationData(getNextBlockGeneratingBalance(lastBlock), signature).asInstanceOf[Constants.ConsensusAlgo.kernelData]))
    } else None
  }

  private def blockGeneratingBalance(block: Block) =
    block.generationData.asInstanceOf[QoraBlockGenerationData].generatingBalance


  def getNextBlockGeneratingBalance(block: Block): Long = {
    if (block.height().get % RETARGET == 0) {
      //GET FIRST BLOCK OF TARGET
      val firstBlock = (1 to RETARGET - 1).foldLeft(block) { case (bl, _) => bl.parent().get }

      //CALCULATE THE GENERATING TIME FOR LAST 10 BLOCKS
      val generatingTime = block.timestamp - firstBlock.timestamp

      //CALCULATE EXPECTED FORGING TIME
      val expectedGeneratingTime = getBlockTime(blockGeneratingBalance(block)) * RETARGET * 1000

      //CALCULATE MULTIPLIER
      val multiplier = expectedGeneratingTime / generatingTime.toDouble

      //CALCULATE NEW GENERATING BALANCE
      val generatingBalance = (blockGeneratingBalance(block) * multiplier).toLong
      minMaxBalance(generatingBalance)
    } else blockGeneratingBalance(block)
  }

  def getBaseTarget(generatingBalance: Long): Long = minMaxBalance(generatingBalance) * getBlockTime(generatingBalance)

  def getBlockTime(generatingBalance: Long): Long = {
    val percentageOfTotal = minMaxBalance(generatingBalance) / MAX_BALANCE.toDouble
    (MIN_BLOCK_TIME + ((MAX_BLOCK_TIME - MIN_BLOCK_TIME) * (1 - percentageOfTotal))).toLong
  }

  def calculateSignature(prevBlock: Block, account: PrivateKeyAccount):Array[Byte] = {
    val gb = getNextBlockGeneratingBalance(prevBlock)
    val ref = prevBlock.generationData.asInstanceOf[QoraBlockGenerationData].generatorSignature
    calculateSignature(ref, gb, account)
  }

  def calculateSignature(reference:Array[Byte],
                                 generatingBalance:Long,
                                 account: PrivateKeyAccount):Array[Byte] = {
    //PARENT GENERATOR SIGNATURE
    val generatorSignature = reference.take(QoraBlockGenerationDataParser.GeneratorSignatureLength)

    val genBalanceBytes = Longs.toByteArray(generatingBalance)
      .ensuring(_.size == QoraBlockGenerationDataParser.GENERATING_BALANCE_LENGTH)

    require(account.publicKey.length == Block.GeneratorLength)
    val si = Bytes.concat(generatorSignature, genBalanceBytes, account.publicKey)
    SigningFunctionsImpl.sign(account, si)
  }

  private def minMaxBalance(generatingBalance: Long) =
    if (generatingBalance < MIN_BALANCE) MIN_BALANCE
    else if (generatingBalance > MAX_BALANCE) MAX_BALANCE
    else generatingBalance
}
