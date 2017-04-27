package scorex.transaction

import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings}
import com.wavesplatform.state2.Validator
import com.wavesplatform.state2.reader.StateReader
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.block.Block
import scorex.consensus.TransactionsOrdering
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.hash.FastCryptographicHash.hash
import scorex.transaction.SimpleTransactionModule._
import scorex.utils.{NTP, ScorexLogging}

import scala.concurrent.duration._

trait TransactionModule {

  def utxStorage: UnconfirmedTransactionsStorage

  def blockStorage: BlockStorage

  def validate[T <: Transaction](tx: T): Either[ValidationError, T]

  def unconfirmedTxs: Seq[Transaction]

  def putUnconfirmedIfNew[T <: Transaction](tx: T): Either[ValidationError, T]

  def clearFromUnconfirmed(data: Seq[Transaction]): Unit

  def onNewOffchainTransaction[T <: Transaction](transaction: T): Either[ValidationError, T]

  def isValid(block: Block): Boolean
}

object TransactionModule extends ScorexLogging {


  def blockOrdering(history: History, state: StateReader, fs: FunctionalitySettings): Ordering[Block] = Ordering.by {
    block =>
      val parent = history.blockById(block.reference).get
      val blockCreationTime = nextBlockGenerationTime(history, state, fs)(parent, block.signerData.generator)
        .getOrElse(block.timestamp)
      (block.blockScore, -blockCreationTime)
  }

  def nextBlockGenerationTime(history: History, state: StateReader, fs: FunctionalitySettings)(block: Block, account: PublicKeyAccount): Option[Long] = {
    history.heightOf(block.uniqueId)
      .map(height => (height, generatingBalance(state, fs)(account, height))).filter(_._2 > 0)
      .flatMap {
        case (height, balance) =>
          val cData = block.consensusData
          val hit = calcHit(cData, account)
          val t = cData.baseTarget

          val result =
            Some((hit * 1000) / (BigInt(t) * balance) + block.timestamp)
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

  def generatingBalance(state: StateReader, fs: FunctionalitySettings)(account: Account, atHeight: Int): Long = {
    val generatingBalanceDepth = if (atHeight >= fs.generatingBalanceDepthFrom50To1000AfterHeight) 1000 else 50
    state.effectiveBalanceAtHeightWithConfirmations(account, atHeight, generatingBalanceDepth)
  }

  def generateNextBlocks(history: History, state: StateReader, bcs: BlockchainSettings, utx: UnconfirmedTransactionsStorage)(accounts: Seq[PrivateKeyAccount]): Seq[Block] =
    accounts.flatMap(generateNextBlock(history, state, bcs, utx) _)

  def generateNextBlock(history: History, state: StateReader, bcs: BlockchainSettings, utx: UnconfirmedTransactionsStorage)
                       (account: PrivateKeyAccount): Option[Block] = try {

    val lastBlock = history.lastBlock
    val height = history.heightOf(lastBlock).get
    val balance = generatingBalance(state, bcs.functionalitySettings)(account, height)

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

      val avgBlockDelay = bcs.genesisSettings.averageBlockDelay
      val btg = calcBaseTarget(history)(avgBlockDelay, lastBlock, currentTime)
      val gs = calcGeneratorSignature(lastBlockKernelData, account)
      val consensusData = NxtLikeConsensusBlockData(btg, gs)

      val unconfirmed = packUnconfirmed(bcs.functionalitySettings, utx, state)(Some(height))
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


  def packUnconfirmed(fs: FunctionalitySettings, utx: UnconfirmedTransactionsStorage, state: StateReader)
                     (heightOpt: Option[Int]): Seq[Transaction] = synchronized {
    clearIncorrectTransactions(fs, state, utx)

    val txs = utx.all()
      .sorted(TransactionsOrdering.InUTXPool)
      .take(MaxTransactionsPerBlock)
      .sorted(TransactionsOrdering.InBlock)

    val valid = Validator.validate(fs, state, txs, heightOpt, NTP.correctedTime())._2

    if (valid.size != txs.size) {
      log.debug(s"Txs for new block do not match: valid=${valid.size} vs all=${txs.size}")
    }

    valid
  }

  def clearIncorrectTransactions(fs: FunctionalitySettings, stateReader: StateReader, utx: UnconfirmedTransactionsStorage): Unit = {
    val currentTime = NTP.correctedTime()
    val txs = utx.all()
    val notExpired = txs.filter { tx => (currentTime - tx.timestamp).millis <= MaxTimeUtxPast }
    val notFromFuture = notExpired.filter { tx => (tx.timestamp - currentTime).millis <= MaxTimeUtxFuture }
    val inOrder = notFromFuture.sorted(TransactionsOrdering.InUTXPool)
    val valid = Validator.validate(fs, stateReader, inOrder, None, currentTime)._2
    txs.diff(valid).foreach(utx.remove)
  }


  private def calcBaseTarget(history: History)(avgBlockDelay: FiniteDuration, prevBlock: Block, timestamp: Long): Long = {

    val avgDelayInSeconds = avgBlockDelay.toSeconds

    def normalize(value: Long): Double = value * avgDelayInSeconds / (60: Double)

    val height = history.heightOf(prevBlock).get
    val prevBaseTarget = prevBlock.consensusDataField.value.baseTarget
    if (height % 2 == 0) {
      val blocktimeAverage = history.parent(prevBlock, AvgBlockTimeDepth - 1)
        .map(b => (timestamp - b.timestamp) / AvgBlockTimeDepth)
        .getOrElse(timestamp - prevBlock.timestamp) / 1000

      val minBlocktimeLimit = normalize(53)
      val maxBlocktimeLimit = normalize(67)
      val baseTargetGamma = normalize(64)
      val maxBaseTarget = Long.MaxValue / avgDelayInSeconds

      val baseTarget = (if (blocktimeAverage > avgDelayInSeconds) {
        prevBaseTarget * Math.min(blocktimeAverage, maxBlocktimeLimit) / avgDelayInSeconds
      } else {
        prevBaseTarget - prevBaseTarget * baseTargetGamma *
          (avgDelayInSeconds - Math.max(blocktimeAverage, minBlocktimeLimit)) / (avgDelayInSeconds * 100)
      }).toLong

      scala.math.min(baseTarget, maxBaseTarget)
    } else {
      prevBaseTarget
    }
  }

  private def calcTarget(prevBlock: Block, timestamp: Long, balance: Long): BigInt = {
    val eta = (timestamp - prevBlock.timestamp) / 1000
    BigInt(prevBlock.consensusData.baseTarget) * eta * balance
  }

  private def calcHit(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKeyAccount): BigInt =
    BigInt(1, calcGeneratorSignature(lastBlockData, generator).take(8).reverse)

  private def calcGeneratorSignature(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKeyAccount) =
    hash(lastBlockData.generationSignature ++ generator.publicKey)
}

