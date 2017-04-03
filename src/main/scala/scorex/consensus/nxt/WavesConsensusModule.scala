package scorex.consensus.nxt

import com.wavesplatform.settings.BlockchainSettings
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.block.Block
import scorex.consensus.TransactionsOrdering
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash._
import scorex.transaction._
import scorex.utils.{NTP, ScorexLogging}

import scala.concurrent.duration._
import scala.util.control.NonFatal

class WavesConsensusModule(val settings: BlockchainSettings) extends ScorexLogging {

  import WavesConsensusModule._

  val genesisData = NxtLikeConsensusBlockData(settings.genesisSettings.initialBaseTarget, EmptySignature)

  private val MinBlocktimeLimit = normalize(53)
  private val MaxBlocktimeLimit = normalize(67)
  private val BaseTargetGamma = normalize(64)
  private val MaxBaseTarget = Long.MaxValue / avgDelayInSeconds

  private def avgDelayInSeconds: Long = settings.genesisSettings.averageBlockDelay.toSeconds

  private def normalize(value: Long): Double = value * avgDelayInSeconds / (60: Double)

  def blockOrdering(implicit transactionModule: TransactionModule): Ordering[(Block)] =
    Ordering.by {
      block =>
        val parent = transactionModule.blockStorage.history.blockById(block.reference).get
        val blockCreationTime = nextBlockGenerationTime(parent, block.signerData.generator)
          .getOrElse(block.timestamp)

        (block.blockScore, -blockCreationTime)
    }

  def isValid(block: Block)(implicit transactionModule: TransactionModule): Boolean = try {
    val blockTime = block.timestampField.value

    require((blockTime - NTP.correctedTime()).millis < MaxTimeDrift, s"Block timestamp $blockTime is from future")

    val history = transactionModule.blockStorage.history

    if (block.timestampField.value > settings.functionalitySettings.requireSortedTransactionsAfter) {
      require(block.transactionDataField.asInstanceOf[TransactionsBlockField].value.sorted(TransactionsOrdering.InBlock) == block.transactionDataField.asInstanceOf[TransactionsBlockField].value, "Transactions must be sorted correctly")
    }

    val parentOpt = history.parent(block)
    require(parentOpt.isDefined || history.height() == 1, s"Can't find parent block with id '${
      Base58.encode(block.referenceField.value)
    }' of block " +
      s"'${
        Base58.encode(block.uniqueId)
      }'")

    val parent = parentOpt.get
    val parentHeightOpt = history.heightOf(parent.uniqueId)
    require(parentHeightOpt.isDefined, s"Can't get parent block with id '${
      Base58.encode(block.referenceField.value)
    }' height")
    val parentHeight = parentHeightOpt.get

    val prevBlockData = parent.consensusDataField.value
    val blockData = block.consensusDataField.value

    //check baseTarget
    val cbt = calcBaseTarget(parent, blockTime)
    val bbt = blockData.baseTarget
    require(cbt == bbt, s"Block's basetarget is wrong, calculated: $cbt, block contains: $bbt")

    val generator = block.signerDataField.value.generator

    //check generation signature
    val calcGs = calcGeneratorSignature(prevBlockData, generator)
    val blockGs = blockData.generationSignature
    require(calcGs.sameElements(blockGs),
      s"Block's generation signature is wrong, calculated: ${
        calcGs.mkString
      }, block contains: ${
        blockGs.mkString
      }")

    val effectiveBalance = generatingBalance(generator, parentHeight)

    if (block.timestampField.value >= settings.functionalitySettings.minimalGeneratingBalanceAfterTimestamp) {
      require(effectiveBalance >= MinimalEffectiveBalanceForGenerator, s"Effective balance $effectiveBalance is less that minimal ($MinimalEffectiveBalanceForGenerator)")
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

  def generateNextBlocks(accounts: Seq[PrivateKeyAccount])
                        (implicit transactionModule: TransactionModule): Seq[Block] =
    accounts.flatMap(generateNextBlock(_))

  def generateNextBlock(account: PrivateKeyAccount)
                       (implicit tm: TransactionModule): Option[Block] = try {

    val history = tm.blockStorage.history

    val lastBlock = history.lastBlock

    val height = history.heightOf(lastBlock).get
    val balance = generatingBalance(account, height)

    if (balance < MinimalEffectiveBalanceForGenerator) {
      throw new IllegalStateException(s"Effective balance $balance is less that minimal ($MinimalEffectiveBalanceForGenerator)")
    }

    val lastBlockKernelData = lastBlock.consensusDataField.value

    val lastBlockTime = lastBlock.timestampField.value

    val currentTime = NTP.correctedTime()

    val h = calcHit(lastBlockKernelData, account)
    val t = calcTarget(lastBlock, currentTime, balance)

    val eta = (currentTime - lastBlockTime) / 1000

    log.debug(s"hit: $h, target: $t, generating ${
      h < t
    }, eta $eta, " +
      s"account:  $account " +
      s"account balance: $balance " +
      s"last block id: ${
        lastBlock.encodedId
      }, " +
      s"height: $height, " +
      s"last block target: ${
        lastBlockKernelData.baseTarget
      }"
    )

    if (h < t) {

      val btg = calcBaseTarget(lastBlock, currentTime)
      val gs = calcGeneratorSignature(lastBlockKernelData, account)
      val consensusData = NxtLikeConsensusBlockData(btg, gs)

      val unconfirmed = tm.packUnconfirmed(Some(height))
      log.debug(s"Build block with ${
        unconfirmed.size
      } transactions")
      log.debug(s"Block time interval is $eta seconds ")

      Some(Block.buildAndSign(Version,
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
      log.debug(s"Failed to generate new block: ${
        e.getMessage
      }")
      None
  }

  def nextBlockGenerationTime(block: Block, account: PublicKeyAccount)
                             (implicit tm: TransactionModule): Option[Long] = {
    val history = tm.blockStorage.history

    history.heightOf(block.uniqueId)
      .map(height => (height, generatingBalance(account, height))).filter(_._2 > 0)
      .flatMap {
        case (height, balance) =>
          val cData = block.consensusDataField.value
          val hit = calcHit(cData, account)
          val t = cData.baseTarget

          val result =
            Some((hit * 1000) / (BigInt(t) * balance) + block.timestampField.value)
              .filter(_ > 0).filter(_ < Long.MaxValue)
              .map(_.toLong)

          log.debug({
            val currentTime = NTP.correctedTime()
            s"Next block gen time: $result " +
              s"in ${
                result.map(t => (t - currentTime) / 1000)
              } seconds, " +
              s"hit: $hit, target: $t, " +
              s"account:  $account, account balance: $balance " +
              s"last block id: ${
                block.encodedId
              }, " +
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
                                (implicit transactionModule: TransactionModule): Long = {
    val history = transactionModule.blockStorage.history
    val height = history.heightOf(prevBlock).get
    val prevBaseTarget = prevBlock.consensusDataField.value.baseTarget
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
                           balance: Long)(implicit transactionModule: TransactionModule): BigInt = {

    require(balance >= 0, s"Balance cannot be negative")

    val prevBlockData = prevBlock.consensusDataField.value
    val prevBlockTimestamp = prevBlock.timestampField.value

    val eta = (timestamp - prevBlockTimestamp) / 1000 //in seconds

    BigInt(prevBlockData.baseTarget) * eta * balance
  }

  def generatingBalance(account: Account, atHeight: Int)
                       (implicit transactionModule: TransactionModule): Long = {
    val balanceSheet = transactionModule.blockStorage.state
    val generatingBalanceDepth =
      if (atHeight >= settings.functionalitySettings.generatingBalanceDepthFrom50To1000AfterHeight) 1000 else 50
    balanceSheet.effectiveBalanceWithConfirmations(account, generatingBalanceDepth, atHeight)
  }
}

object WavesConsensusModule {
  val BaseTargetLength: Int = 8
  val GeneratorSignatureLength: Int = 32
  val MinimalEffectiveBalanceForGenerator: Long = 1000000000000L
  val AvgBlockTimeDepth: Int = 3
  val MaxTimeDrift: FiniteDuration = 15.seconds
  val EmptySignature: Array[Byte] = Array.fill(DigestSize)(0: Byte)
  val Version: Byte = 2
}
