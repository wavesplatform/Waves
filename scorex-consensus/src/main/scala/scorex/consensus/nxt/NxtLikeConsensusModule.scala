package scorex.consensus.nxt

import com.google.common.primitives.Longs
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.block.{Block, BlockField}
import scorex.consensus.{ConsensusModule, OneGeneratorConsensusModule, PoSConsensusModule, TransactionsOrdering}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash._
import scorex.settings.WavesHardForkParameters
import scorex.transaction._
import scorex.utils.{NTP, ScorexLogging}

import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal

class NxtLikeConsensusModule(override val forksConfig: WavesHardForkParameters, AvgDelay: Duration = 5.seconds) extends PoSConsensusModule[NxtLikeConsensusBlockData]
with OneGeneratorConsensusModule with ScorexLogging {

  import NxtLikeConsensusModule._

  implicit val consensusModule: ConsensusModule[NxtLikeConsensusBlockData] = this

  val version = 2: Byte

  val MinBlocktimeLimit = normalize(53)
  val MaxBlocktimeLimit = normalize(67)
  val BaseTargetGamma = normalize(64)
  val MaxBaseTarget = Long.MaxValue / avgDelayInSeconds
  val InitialBaseTarget = MaxBaseTarget / 2

  private def avgDelayInSeconds: Long = AvgDelay.toSeconds

  private def normalize(value: Long): Double = value * avgDelayInSeconds / (60: Double)

  override def isValid[TT](block: Block)(implicit transactionModule: TransactionModule[TT]): Boolean = try {
    val blockTime = block.timestampField.value

    require((blockTime - NTP.correctedTime()).millis < MaxTimeDrift, s"Block timestamp $blockTime is from future")

    val history = transactionModule.blockStorage.history

    if (block.timestampField.value > forksConfig.requireSortedTransactionsAfter) {
      require(block.transactions.sorted(TransactionsOrdering) == block.transactions, "Transactions must be sorted correctly")
    }

    val parentOpt = history.parent(block)
    require(parentOpt.isDefined || history.height() == 1, s"Can't find parent block with id '${Base58.encode(block.referenceField.value)}' of block " +
      s"'${Base58.encode(block.uniqueId)}'")

    val parent = parentOpt.get
    val parentHeightOpt = history.heightOf(parent.uniqueId)
    require(parentHeightOpt.isDefined, s"Can't get parent block with id '${Base58.encode(block.referenceField.value)}' height")
    val parentHeight = parentHeightOpt.get

    val prevBlockData = consensusBlockData(parent)
    val blockData = consensusBlockData(block)

    //check baseTarget
    val cbt = calcBaseTarget(parent, blockTime)
    val bbt = blockData.baseTarget
    require(cbt == bbt, s"Block's basetarget is wrong, calculated: $cbt, block contains: $bbt")

    val generator = block.signerDataField.value.generator

    //check generation signature
    val calcGs = calcGeneratorSignature(prevBlockData, generator)
    val blockGs = blockData.generationSignature
    require(calcGs.sameElements(blockGs),
      s"Block's generation signature is wrong, calculated: ${calcGs.mkString}, block contains: ${blockGs.mkString}")

    val effectiveBalance = generatingBalance(generator, Some(parentHeight))

    if (block.timestampField.value >= forksConfig.minimalGeneratingBalanceAfterTimestamp) {
      require(effectiveBalance >= MinimalEffictiveBalanceForGenerator, s"Effective balance $effectiveBalance is less that minimal ($MinimalEffictiveBalanceForGenerator)")
    }

    //check hit < target
    calcHit(prevBlockData, generator) < calcTarget(parent, blockTime, effectiveBalance)
  } catch {
    case e: IllegalArgumentException =>
      log.error("Error while checking a block", e)
      false
    case NonFatal(t) =>
      log.error("Fatal error while checking a block", t)
      throw t
  }

  override def generateNextBlock[TT](account: PrivateKeyAccount)
                                    (implicit tm: TransactionModule[TT]): Option[Block] = try {

    val history = tm.blockStorage.history

    val lastBlock = history.lastBlock

    val height = history.heightOf(lastBlock).get
    val balance = generatingBalance(account, Some(height))

    if (balance < MinimalEffictiveBalanceForGenerator) {
      throw new IllegalStateException(s"Effective balance $balance is less that minimal ($MinimalEffictiveBalanceForGenerator)")
    }

    val lastBlockKernelData = consensusBlockData(lastBlock)

    val lastBlockTime = lastBlock.timestampField.value

    val currentTime = NTP.correctedTime()

    val h = calcHit(lastBlockKernelData, account)
    val t = calcTarget(lastBlock, currentTime, balance)

    val eta = (currentTime - lastBlockTime) / 1000

    log.debug(s"hit: $h, target: $t, generating ${h < t}, eta $eta, " +
      s"account:  $account " +
      s"account balance: $balance " +
      s"last block id: ${lastBlock.encodedId}, " +
      s"height: $height, " +
      s"last block target: ${lastBlockKernelData.baseTarget}"
    )

    if (h < t) {

      val btg = calcBaseTarget(lastBlock, currentTime)
      val gs = calcGeneratorSignature(lastBlockKernelData, account)
      val consensusData = new NxtLikeConsensusBlockData {
        override val generationSignature: Array[Byte] = gs
        override val baseTarget: Long = btg
      }

      val unconfirmed = tm.packUnconfirmed(Some(height))
      log.debug(s"Build block with ${unconfirmed.asInstanceOf[Seq[Transaction]].size} transactions")
      log.debug(s"Block time interval is $eta seconds ")

      Some(Block.buildAndSign(version,
        currentTime,
        lastBlock.uniqueId,
        consensusData,
        unconfirmed,
        account))
    } else None
  } catch {
    case e: UnsupportedOperationException =>
      log.debug(s"DB can't find last block because of unexpected modification")
      None
    case e: IllegalStateException =>
      log.debug(s"Failed to generate new block: ${e.getMessage}")
      None
  }

  override def nextBlockGenerationTime(block: Block, account: PublicKeyAccount)
                                      (implicit tm: TransactionModule[_]): Option[Long] = {
    val history = tm.blockStorage.history

    history.heightOf(block.uniqueId)
      .map(height => (height, generatingBalance(account, Some(height)))).filter(_._2 > 0)
      .flatMap { case (height, balance) =>
        val cData = consensusBlockData(block)
        val hit = calcHit(cData, account)
        val t = cData.baseTarget

        val result =
          Some((hit * 1000) / (BigInt(t) * balance) + block.timestampField.value)
            .filter(_ > 0).filter(_ < Long.MaxValue)
            .map(_.toLong)

        log.debug({
          val currentTime = NTP.correctedTime()
          s"Next block gen time: $result " +
            s"in ${result.map(t => (t - currentTime) / 1000)} seconds, " +
            s"hit: $hit, target: $t, " +
            s"account:  $account, account balance: $balance " +
            s"last block id: ${block.encodedId}, " +
            s"height: $height"
        })

        result
      }
  }

  private def calcGeneratorSignature(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKeyAccount) =
    hash(lastBlockData.generationSignature ++ generator.publicKey)

  private def calcHit(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKeyAccount): BigInt =
    BigInt(1, calcGeneratorSignature(lastBlockData, generator).take(8).reverse)

  /**
   * BaseTarget calculation algorithm fixing the blocktimes.
   */
  private def calcBaseTarget[TT](prevBlock: Block, timestamp: Long)
                                (implicit transactionModule: TransactionModule[TT]): Long = {
    val history = transactionModule.blockStorage.history
    val height = history.heightOf(prevBlock).get
    val prevBaseTarget = consensusBlockData(prevBlock).baseTarget
    if (height % 2 == 0) {
      val blocktimeAverage = history.parent(prevBlock, AvgBlockTimeDepth - 1)
        .map(b => (timestamp - b.timestampField.value) / AvgBlockTimeDepth)
        .getOrElse(timestamp - prevBlock.timestampField.value) / 1000

      val baseTarget = (if (blocktimeAverage > avgDelayInSeconds) {
        prevBaseTarget * Math.min(blocktimeAverage, MaxBlocktimeLimit) / avgDelayInSeconds
      } else {
        prevBaseTarget - prevBaseTarget * BaseTargetGamma *
          (avgDelayInSeconds - Math.max(blocktimeAverage, MinBlocktimeLimit)) / (avgDelayInSeconds * 100)
      }).toLong

      // TODO: think about MinBaseTarget like in Nxt
      scala.math.min(baseTarget, MaxBaseTarget)
    } else {
      prevBaseTarget
    }
  }

  protected def calcTarget(prevBlock: Block,
                           timestamp: Long,
                           balance: Long)(implicit transactionModule: TransactionModule[_]): BigInt = {

    require(balance >= 0, s"Balance cannot be negative")

    val prevBlockData = consensusBlockData(prevBlock)
    val prevBlockTimestamp = prevBlock.timestampField.value

    val eta = (timestamp - prevBlockTimestamp) / 1000 //in seconds

    BigInt(prevBlockData.baseTarget) * eta * balance
  }

  override def parseBytes(bytes: Array[Byte]): Try[BlockField[NxtLikeConsensusBlockData]] = Try {
    NxtConsensusBlockField(new NxtLikeConsensusBlockData {
      override val baseTarget: Long = Longs.fromByteArray(bytes.take(BaseTargetLength))
      override val generationSignature: Array[Byte] = bytes.takeRight(GeneratorSignatureLength)
    })
  }

  override def blockScore(block: Block): BigInt = {
    val baseTarget = consensusBlockData(block).baseTarget
    BigInt("18446744073709551616") / baseTarget
  }.ensuring(_ > 0)

  override def generators(block: Block): Seq[Account] = Seq(block.signerDataField.value.generator)

  override def genesisData: BlockField[NxtLikeConsensusBlockData] =
    NxtConsensusBlockField(new NxtLikeConsensusBlockData {
      override val baseTarget: Long = InitialBaseTarget
      override val generationSignature: Array[Byte] = Array.fill(32)(0: Byte)
    })

  override def formBlockData(data: NxtLikeConsensusBlockData): BlockField[NxtLikeConsensusBlockData] =
    NxtConsensusBlockField(data)

  override def consensusBlockData(block: Block): NxtLikeConsensusBlockData = block.consensusDataField.value match {
    case b: NxtLikeConsensusBlockData => b
    case m => throw new AssertionError(s"Only NxtLikeConsensusBlockData is available, $m given")
  }
}

object NxtLikeConsensusModule {
  val BaseTargetLength = 8
  val GeneratorSignatureLength = 32
  // 10000 waves
  val MinimalEffictiveBalanceForGenerator = 1000000000000L

  val AvgBlockTimeDepth: Int = 3
  val MaxTimeDrift = 15.seconds
}
