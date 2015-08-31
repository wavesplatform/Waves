package scorex.consensus.nxt

import com.google.common.primitives.Longs
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.block.{Block, BlockField}
import scorex.consensus.{ConsensusModule, LagonakiConsensusModule}
import scorex.crypto.Sha256._
import scorex.transaction._
import scorex.utils.{NTP, ScorexLogging}

import scala.util.Try

class NxtLikeConsensusModule
  extends LagonakiConsensusModule[NxtLikeConsensusBlockData] with ScorexLogging {

  implicit val consensusModule: ConsensusModule[NxtLikeConsensusBlockData] = this

  val BaseTargetLength = 8

  val GeneratorSignatureLength = 32

  val AvgFrequency = 2

  //60 - the algo's goal is 1 block per minute in average
  val version = 1: Byte

  def isValid[TT](block: Block, history: History, state: State)
                 (implicit transactionModule: TransactionModule[TT]): Boolean = Try {

    val blockTime = block.timestampField.value

    val prev = history.parent(block).get
    val prevTime = prev.timestampField.value

    val consensusBlockData = prev.consensusDataField.asInstanceOf[NxtLikeConsensusBlockData]
    val generator = block.signerDataField.value.generator

    //check baseTarget
    val cbt = calcBaseTarget(consensusBlockData, prevTime, blockTime)
    require(cbt == consensusBlockData.baseTarget, "Block's basetarget is wrong")

    //check generation signature
    val calcGs = calcGeneratorSignature(consensusBlockData, generator)
    require(calcGs.sameElements(consensusBlockData.generationSignature), "Block's generation signature is wrong")

    //check hit < target
    calcHit(consensusBlockData, generator) < calcTarget(consensusBlockData, prevTime, generator)
  }.getOrElse(false)


  override def generateNextBlock[TT](account: PrivateKeyAccount,
                                     state: State,
                                     history: History)
                                    (implicit transactionModule: TransactionModule[TT]): Option[Block] = {

    val lastBlock = history.asInstanceOf[BlockChain].lastBlock
    val lastBlockKernelData = lastBlock.consensusDataField.asInstanceOf[NxtLikeConsensusBlockData]

    val lastBlockTime = lastBlock.timestampField.value

    val h = calcHit(lastBlockKernelData, account)
    val t = calcTarget(lastBlockKernelData, lastBlockTime, account)

    val eta = (NTP.correctedTime() - lastBlockTime) / 1000

    log.debug(s"hit: $h, target: $t, generating ${h < t}, eta $eta, " +
      s"account balance: ${transactionModule.asInstanceOf[BalanceSheet].generationBalance(account)}")

    if (h < t) {
      val timestamp = NTP.correctedTime()
      val btg = calcBaseTarget(lastBlockKernelData, lastBlockTime, timestamp)
      val gs = calcGeneratorSignature(lastBlockKernelData, account)
      val consensusData = new NxtLikeConsensusBlockData {
        override val generationSignature: Array[Byte] = gs
        override val baseTarget: Long = btg
      }

      None
      //todo: Some(Block(version, timestamp, lastBlock.uniqueId, consensusData, transactionModule.packUnconfirmed()))

    } else None
  }

  private def calcGeneratorSignature(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKeyAccount) =
    hash(lastBlockData.generationSignature ++ generator.publicKey)

  private def calcHit(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKeyAccount): BigInt =
    BigInt(1, calcGeneratorSignature(lastBlockData, generator).take(8))

  private def calcBaseTarget(lastBlockData: NxtLikeConsensusBlockData,
                             lastBlockTimestamp: Long,
                             currentTime: Long): Long = {
    val eta = (currentTime - lastBlockTimestamp) / 1000 //in seconds
    val prevBt = BigInt(lastBlockData.baseTarget)
    val t = bounded(prevBt * eta / AvgFrequency, prevBt / 2, prevBt * 2)
    bounded(t, 1, Long.MaxValue).toLong
  }

  private def calcTarget(lastBlockData: NxtLikeConsensusBlockData,
                         lastBlockTimestamp: Long,
                         generator: PublicKeyAccount)(implicit transactionModule: TransactionModule[_]): BigInt = {
    val eta = (NTP.correctedTime() - lastBlockTimestamp) / 1000 //in seconds
    val effBalance: BigDecimal = transactionModule.asInstanceOf[BalanceSheet].generationBalance(generator)
    (lastBlockData.baseTarget * eta * effBalance).toBigInt()
  }

  private def bounded(value: BigInt, min: BigInt, max: BigInt): BigInt =
    if (value < min) min else if (value > max) max else value

  override def parseBlockData(bytes: Array[Byte]): BlockField[NxtLikeConsensusBlockData] =
    NxtConsensusBlockField(new NxtLikeConsensusBlockData {
      override val baseTarget: Long = Longs.fromByteArray(bytes.take(BaseTargetLength))
      override val generationSignature: Array[Byte] = bytes.slice(BaseTargetLength, GeneratorSignatureLength)
    })

  override def blockScore(block: Block, history: History)
                         (implicit transactionModule: TransactionModule[_]): BigInt = {
    val baseTarget = block.consensusDataField.asInstanceOf[NxtLikeConsensusBlockData].baseTarget
    BigInt("18446744073709551616") / baseTarget
  }

  override def generators(block: Block): Seq[Account] = Seq(block.signerDataField.value.generator)

  override def genesisData: BlockField[NxtLikeConsensusBlockData] =
    NxtConsensusBlockField(new NxtLikeConsensusBlockData {
      override val baseTarget: Long = 153722867
      override val generationSignature: Array[Byte] = Array.fill(32)(0: Byte)
    })
}
