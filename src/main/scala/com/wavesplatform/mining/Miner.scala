package com.wavesplatform.mining

import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures, FeatureProvider}
import com.wavesplatform.metrics.{BlockStats, HistogramExt, Instrumented}
import com.wavesplatform.network._
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2._
import com.wavesplatform.{Coordinator, UtxPool}
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import kamon.metric.instrument
import monix.eval.Task
import monix.execution._
import monix.execution.cancelables.{CompositeCancelable, SerialCancelable}
import monix.execution.schedulers.SchedulerService
import scorex.account.{Address, PrivateKeyAccount}
import scorex.block.Block._
import scorex.block.{Block, MicroBlock}
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction.PoSCalc._
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}
import scorex.wallet.Wallet

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.control.NoStackTrace

trait Miner {
  def scheduleMining(): Unit
}

trait MinerDebugInfo {
  def collectNextBlockGenerationTimes: List[(Address, Long)]
}

class MinerImpl(
                   allChannels: ChannelGroup,
                   blockchainReadiness: AtomicBoolean,
                   blockchainUpdater: BlockchainUpdater,
                   checkpoint: CheckpointService,
                   history: NgHistory,
                   featureProvider: FeatureProvider,
                   stateReader: StateReader,
                   settings: WavesSettings,
                   timeService: Time,
                   utx: UtxPool,
                   wallet: Wallet,
                   coordinatorScheduler: Scheduler) extends Miner with MinerDebugInfo with ScorexLogging with Instrumented {

  import Miner._

  private implicit val scheduler: SchedulerService = Scheduler.fixedPool(name = "miner-pool", poolSize = 2)

  private lazy val minerSettings = settings.minerSettings
  private lazy val minMicroBlockDurationMills = minerSettings.minMicroBlockAge.toMillis
  private lazy val blockchainSettings = settings.blockchainSettings
  private lazy val processBlock = Coordinator
    .processSingleBlock(checkpoint, history, blockchainUpdater, timeService, stateReader, utx, blockchainSettings, featureProvider) _

  private val scheduledAttempts = SerialCancelable()
  private val microBlockAttempt = SerialCancelable()

  private val blockBuildTimeStats = Kamon.metrics.histogram("pack-and-forge-block-time", instrument.Time.Milliseconds)
  private val microBlockBuildTimeStats = Kamon.metrics.histogram("forge-microblock-time", instrument.Time.Milliseconds)

  private val nextBlockGenerationTimes: MMap[Address, Long] = MMap.empty

  def collectNextBlockGenerationTimes: List[(Address, Long)] = Await.result(Task.now(nextBlockGenerationTimes.toList).runAsyncLogErr, Duration.Inf)

  private def checkAge(parentHeight: Int, parentTimestamp: Long): Either[String, Unit] =
    Either.cond(parentHeight == 1, (), (timeService.correctedTime() - parentTimestamp).millis)
      .left.flatMap(blockAge => Either.cond(blockAge <= minerSettings.intervalAfterLastBlockThenGenerationIsAllowed, (),
      s"BlockChain is too old (last block timestamp is $parentTimestamp generated $blockAge ago)"
    ))

  private def ngEnabled: Boolean = featureProvider.featureActivationHeight(BlockchainFeatures.NG.id).exists(history.height > _ + 1)

  private def generateOneBlockTask(account: PrivateKeyAccount, balance: Long)
                                  (delay: FiniteDuration): Task[Block] = Task {
    microBlockAttempt.cancel()
    val (height, version, greatGrandParentTimestamp, referencedBlockInfo) = history.read { implicit l =>
      val height = history.height()
      val version = if (height <= blockchainSettings.functionalitySettings.blockVersion3AfterHeight) PlainBlockVersion else NgBlockVersion
      val lastBlock = history.lastBlock.get
      val greatGrandParentTimestamp = history.parent(lastBlock, 2).map(_.timestamp)
      val referencedBlockInfo = history.bestLastBlockInfo(System.currentTimeMillis() - minMicroBlockDurationMills).get
      (height, version, greatGrandParentTimestamp, referencedBlockInfo)
    }

    val pc = allChannels.size()

    check(pc >= minerSettings.quorum, s"Quorum not available ($pc/${minerSettings.quorum}, not forging block with $account}")
    val currentTime = timeService.correctedTime()
    val h = calcHit(referencedBlockInfo.consensus, account)
    val t = calcTarget(referencedBlockInfo.timestamp, referencedBlockInfo.consensus.baseTarget, currentTime, balance)
    check(h < t, s"${System.currentTimeMillis()}: Hit $h was NOT less than target $t, not forging block with $account}")
    log.debug(s"Forging with $account, H $h < T $t, balance $balance, prev block ${referencedBlockInfo.blockId}")
    log.debug(s"Previous block ID ${referencedBlockInfo.blockId} at $height with target ${referencedBlockInfo.consensus.baseTarget}")

    val avgBlockDelay = blockchainSettings.genesisSettings.averageBlockDelay
    val btg = calcBaseTarget(avgBlockDelay, height, referencedBlockInfo.consensus.baseTarget, referencedBlockInfo.timestamp, greatGrandParentTimestamp, currentTime)
    val gs = calcGeneratorSignature(referencedBlockInfo.consensus, account)
    val consensusData = NxtLikeConsensusBlockData(btg, ByteStr(gs))
    val sortInBlock = history.height() <= blockchainSettings.functionalitySettings.dontRequireSortedTransactionsAfter
    val txAmount = if (ngEnabled) minerSettings.maxTransactionsInKeyBlock else ClassicAmountOfTxsInBlock
    val unconfirmed = utx.packUnconfirmed(txAmount, sortInBlock)

    val features = if (version <= 2) Set.empty[Short] else settings
      .featuresSettings
      .supported
      .filter(featureProvider.featureStatus(_, height) == BlockchainFeatureStatus.Undefined)
      .toSet.intersect(BlockchainFeatures.implemented)

    log.debug(s"Adding ${unconfirmed.size} unconfirmed transaction(s) to new block")
    Block.buildAndSign(version, currentTime, referencedBlockInfo.blockId, consensusData, unconfirmed, account, features)
        .left.map(MinerException).toTry.get
  }.delayExecution(delay)

  private def appendMicroBlockTask(microBlock: MicroBlock): Task[Unit] =
    Task(Coordinator.processMicroBlock(checkpoint, history, blockchainUpdater, utx)(microBlock)
        .left.map(MinerException).toTry.get
    ).executeOn(coordinatorScheduler)

  private def logMicroBlockTask(account: PrivateKeyAccount, microBlock: MicroBlock): Task[Unit] = Task {
    BlockStats.mined(microBlock)
    log.trace(s"$microBlock has been mined for $account}")
    allChannels.broadcast(MicroBlockInv(account, microBlock.totalResBlockSig, microBlock.prevResBlockSig))
  }

  private def generateOneMicroBlockTask(account: PrivateKeyAccount, accumulatedBlock: Block): Task[Option[(Block, MicroBlock)]] = Task {
    log.trace(s"Forging microblock with $account")
    val pc = allChannels.size()
    lazy val unconfirmed = measureLog("packing unconfirmed transactions for microblock") {
      utx.packUnconfirmed(minerSettings.maxTransactionsInMicroBlock, sortInBlock = false)
    }
    if (pc < minerSettings.quorum) {
      log.trace(s"Quorum not available ($pc/${minerSettings.quorum}, not forging microblock with $account")
      None
    } else if (unconfirmed.isEmpty) {
      log.trace("skipping microBlock because no txs in utx pool")
      None
    } else {
      log.trace(s"Accumulated ${unconfirmed.size} txs for microblock")
      val start = System.currentTimeMillis()
      (for {
        signedBlock <- Block.buildAndSign(
          version = 3,
          timestamp = accumulatedBlock.timestamp,
          reference = accumulatedBlock.reference,
          consensusData = accumulatedBlock.consensusData,
          transactionData = accumulatedBlock.transactionData ++ unconfirmed,
          signer = account,
          featureVotes = accumulatedBlock.featureVotes
        )
        microBlock <- MicroBlock.buildAndSign(account, unconfirmed, accumulatedBlock.signerData.signature, signedBlock.signerData.signature)
        _ = microBlockBuildTimeStats.safeRecord(System.currentTimeMillis() - start)
      } yield Some((signedBlock, microBlock))).left.map(MinerException).toTry.get
    }
  }

  def generateMicroBlock(account: PrivateKeyAccount, accumulatedBlock: Block) =
    generateOneMicroBlockTask(account, accumulatedBlock).flatMap {
      case Some((newSignedBlock, newMicroBlock)) =>
        for {
          _ <- appendMicroBlockTask(newMicroBlock)
          _ <- logMicroBlockTask(account, newMicroBlock)
        } yield Some(newSignedBlock)
      case None => Task.now(None)
    }

  private def generateMicroBlockSequence(account: PrivateKeyAccount, accumulatedBlock: Block, delay: FiniteDuration): Task[Option[Block]] = {
    generateMicroBlock(account, accumulatedBlock).delayExecution(delay).flatMap { maybeNewBlock =>
      generateMicroBlock(account, maybeNewBlock.getOrElse(accumulatedBlock))
    }
  }

  private def appendBlockTask(block: Block): Task[Option[BigInt]] = Task {
    processBlock(block).left.map(MinerException).toTry.get
  }.executeOn(coordinatorScheduler)

  private def handleBlockAppended(newBlock: Block, maybeNewScore: Option[BigInt], account: PrivateKeyAccount): Task[Unit] =
    Task(maybeNewScore match {
      case None =>
        log.warn("Newly created block has already been appended, should not happen")
      case Some(score) =>
        BlockStats.mined(newBlock, history.height())
        Coordinator.updateBlockchainReadinessFlag(history, timeService, blockchainReadiness, minerSettings.intervalAfterLastBlockThenGenerationIsAllowed)
        allChannels.broadcast(BlockForged(newBlock))
        allChannels.broadcast(LocalScoreChanged(score, false))
        scheduleMining()
        if (ngEnabled)
          startMicroBlockMining(account, newBlock)
    })

  private def generateBlock(account: PrivateKeyAccount) = for {
    newBlock <- generateBlockTask(account)
    maybeNewScore <- appendBlockTask(newBlock)
    _ <- handleBlockAppended(newBlock, maybeNewScore, account)
  } yield ()

  private def generateBlockTask(account: PrivateKeyAccount): Task[Block] = {
    history.read { implicit l =>
      val height = history.height()
      val lastBlock = history.lastBlock.get
      for {
        _ <- checkAge(height, history.lastBlockTimestamp().get)
        balanceAndTs <- nextBlockGenerationTime(height, stateReader, blockchainSettings.functionalitySettings, lastBlock, account, featureProvider)
        (balance, ts) = balanceAndTs
      } yield (calcOffset(timeService, ts, minerSettings.minimalBlockGenerationOffset), balance)
    } match {
      case Right((offset, balance)) =>
        log.debug(s"Next attempt for acc=$account in $offset")
        nextBlockGenerationTimes += account.toAddress -> (System.currentTimeMillis() + offset.toMillis)
        generateOneBlockTask(account, balance)(offset).onErrorRecoverWith {
          case MinerException(e) =>
            log.warn(s"Error forging block: $e")
            generateBlockTask(account)
        }
      case Left(err) =>
        log.debug(s"Not scheduling block forging because $err")
        Task.raiseError(MinerException(GenericError(err)))
    }
  }

  def scheduleMining(): Unit = {
    Miner.blockMiningStarted.increment()
    scheduledAttempts := CompositeCancelable.fromSet(
      wallet.privateKeyAccounts().map(generateBlock).map(_.runAsyncLogErr).toSet)
    microBlockAttempt := SerialCancelable()
  }

  private def startMicroBlockMining(account: PrivateKeyAccount, lastBlock: Block): Unit = {
    Miner.microMiningStarted.increment()
    microBlockAttempt := generateMicroBlockSequence(account, lastBlock, Duration.Zero).runAsyncLogErr
    log.trace(s"MicroBlock forging scheduled for $account")
  }
}

object Miner {
  val blockMiningStarted = Kamon.metrics.counter("block-mining-started")
  val microMiningStarted = Kamon.metrics.counter("micro-mining-started")

  val MaxTransactionsPerMicroblock: Int = 500
  val ClassicAmountOfTxsInBlock: Int = 100

  val Disabled = new Miner with MinerDebugInfo {
    override def scheduleMining(): Unit = ()

    override def collectNextBlockGenerationTimes: List[(Address, Long)] = List.empty
  }

  case class MinerException(cause: ValidationError) extends Exception(cause.toString) with NoStackTrace

  def check(condition: => Boolean, message: String) = if (!condition) throw MinerException(GenericError(message))

  def calcOffset(timeService: Time, calculatedTimestamp: Long, minimalBlockGenerationOffset: FiniteDuration): FiniteDuration = {
    val calculatedGenerationTimestamp = (Math.ceil(calculatedTimestamp / 1000.0) * 1000).toLong
    val calculatedOffset = calculatedGenerationTimestamp - timeService.correctedTime()
    Math.max(minimalBlockGenerationOffset.toMillis, calculatedOffset).millis
  }
}
