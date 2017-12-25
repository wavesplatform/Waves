package com.wavesplatform.mining

import cats.data.EitherT
import com.wavesplatform.UtxPool
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures, FeatureProvider}
import com.wavesplatform.metrics.{BlockStats, HistogramExt, Instrumented}
import com.wavesplatform.network._
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.appender.{BlockAppender, MicroblockAppender}
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
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}
import scorex.wallet.Wallet

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.Await
import scala.concurrent.duration._
import MinerDebugInfo.State

trait Miner {
  def scheduleMining(): Unit
}

trait MinerDebugInfo {
  def state: State.Value
  def collectNextBlockGenerationTimes: List[(Address, Long)]
}

object MinerDebugInfo {
  object State extends Enumeration {
    val Disabled = Value("disabled")
    val Error = Value("error")
    val Mining_Blocks = Value("mining blocks")
    val Mining_Micros = Value("mining microblocks")
  }
}

class MinerImpl(
                   allChannels: ChannelGroup,
                   blockchainUpdater: BlockchainUpdater,
                   checkpoint: CheckpointService,
                   history: NgHistory,
                   featureProvider: FeatureProvider,
                   stateReader: StateReader,
                   settings: WavesSettings,
                   timeService: Time,
                   utx: UtxPool,
                   wallet: Wallet) extends Miner with MinerDebugInfo with ScorexLogging with Instrumented {

  import Miner._

  private implicit val scheduler: SchedulerService = Scheduler.fixedPool(name = "miner-pool", poolSize = 2)

  private lazy val minerSettings = settings.minerSettings
  private lazy val minMicroBlockDurationMills = minerSettings.minMicroBlockAge.toMillis
  private lazy val blockchainSettings = settings.blockchainSettings

  private val scheduledAttempts = SerialCancelable()
  private val microBlockAttempt = SerialCancelable()

  private val blockBuildTimeStats = Kamon.metrics.histogram("pack-and-forge-block-time", instrument.Time.Milliseconds)
  private val microBlockBuildTimeStats = Kamon.metrics.histogram("forge-microblock-time", instrument.Time.Milliseconds)

  private val nextBlockGenerationTimes: MMap[Address, Long] = MMap.empty

  @volatile private var debugState: State.Value = State.Disabled

  def collectNextBlockGenerationTimes: List[(Address, Long)] = Await.result(Task.now(nextBlockGenerationTimes.toList).runAsyncLogErr, Duration.Inf)

  private def checkAge(parentHeight: Int, parentTimestamp: Long): Either[String, Unit] =
    Either.cond(parentHeight == 1, (), (timeService.correctedTime() - parentTimestamp).millis)
      .left.flatMap(blockAge => Either.cond(blockAge <= minerSettings.intervalAfterLastBlockThenGenerationIsAllowed, (),
      s"BlockChain is too old (last block timestamp is $parentTimestamp generated $blockAge ago)"
    ))

  private def ngEnabled: Boolean = featureProvider.featureActivationHeight(BlockchainFeatures.NG.id).exists(history.height > _ + 1)

  private def generateOneBlockTask(account: PrivateKeyAccount, balance: Long)(delay: FiniteDuration): Task[Either[String, Block]] = Task {
    history.read { implicit l =>
      // should take last block right at the time of mining since microblocks might have been added
      val height = history.height()
      val version = if (height <= blockchainSettings.functionalitySettings.blockVersion3AfterHeight) PlainBlockVersion else NgBlockVersion
      val lastBlock = history.lastBlock.get
      val greatGrandParentTimestamp = history.parent(lastBlock, 2).map(_.timestamp)
      val referencedBlockInfo = history.bestLastBlockInfo(System.currentTimeMillis() - minMicroBlockDurationMills).get
      val pc = allChannels.size()
      lazy val currentTime = timeService.correctedTime()
      lazy val h = calcHit(referencedBlockInfo.consensus, account)
      lazy val t = calcTarget(referencedBlockInfo.timestamp, referencedBlockInfo.consensus.baseTarget, currentTime, balance)
      measureSuccessful(blockBuildTimeStats, for {
        _ <- Either.cond(pc >= minerSettings.quorum, (), s"Quorum not available ($pc/${minerSettings.quorum}, not forging block with ${account.address}")
        _ <- Either.cond(h < t, (), s"${System.currentTimeMillis()}: Hit $h was NOT less than target $t, not forging block with ${account.address}")
        _ = log.debug(s"Forging with ${account.address}, H $h < T $t, balance $balance, prev block ${referencedBlockInfo.blockId}")
        _ = log.debug(s"Previous block ID ${referencedBlockInfo.blockId} at $height with target ${referencedBlockInfo.consensus.baseTarget}")
        block <- {
          val avgBlockDelay = blockchainSettings.genesisSettings.averageBlockDelay
          val btg = calcBaseTarget(avgBlockDelay, height, referencedBlockInfo.consensus.baseTarget, referencedBlockInfo.timestamp, greatGrandParentTimestamp, currentTime)
          val gs = calcGeneratorSignature(referencedBlockInfo.consensus, account)
          val consensusData = NxtLikeConsensusBlockData(btg, ByteStr(gs))
          val sortInBlock = history.height() <= blockchainSettings.functionalitySettings.dontRequireSortedTransactionsAfter
          val txAmount = if (ngEnabled) minerSettings.maxTransactionsInKeyBlock else ClassicAmountOfTxsInBlock
          val unconfirmed = utx.packUnconfirmed(txAmount, sortInBlock)

          val features = if (version > 2) settings.featuresSettings.supported
            .filter(featureProvider.featureStatus(_, height) == BlockchainFeatureStatus.Undefined)
            .toSet.intersect(BlockchainFeatures.implemented) else Set.empty[Short]

          log.debug(s"Adding ${unconfirmed.size} unconfirmed transaction(s) to new block")
          Block.buildAndSign(version.toByte, currentTime, referencedBlockInfo.blockId, consensusData, unconfirmed, account, features)
            .left.map(l => l.err)
        }
      } yield block)
    }
  }.delayExecution(delay)


  private def generateOneMicroBlockTask(account: PrivateKeyAccount, accumulatedBlock: Block): Task[Either[ValidationError, Option[Block]]] = Task {
    log.trace(s"Generating microblock for $account")
    val pc = allChannels.size()
    lazy val unconfirmed = measureLog("packing unconfirmed transactions for microblock") {
      utx.packUnconfirmed(settings.minerSettings.maxTransactionsInMicroBlock, sortInBlock = false)
    }
    if (pc < minerSettings.quorum) {
      log.trace(s"Quorum not available ($pc/${minerSettings.quorum}, not forging microblock with ${account.address}")
      Task.now(Right(None))
    }
    else if (unconfirmed.isEmpty) {
      log.trace("skipping microBlock because no txs in utx pool")
      Task.now(Right(None))
    }
    else {
      log.trace(s"Accumulated ${unconfirmed.size} txs for microblock")
      val start = System.currentTimeMillis()
      (for {
        signedBlock <- EitherT(Task.now(Block.buildAndSign(
          version = 3,
          timestamp = accumulatedBlock.timestamp,
          reference = accumulatedBlock.reference,
          consensusData = accumulatedBlock.consensusData,
          transactionData = accumulatedBlock.transactionData ++ unconfirmed,
          signer = account,
          featureVotes = accumulatedBlock.featureVotes
        )))
        microBlock <- EitherT(Task.now(MicroBlock.buildAndSign(account, unconfirmed, accumulatedBlock.signerData.signature, signedBlock.signerData.signature)))
        _ = microBlockBuildTimeStats.safeRecord(System.currentTimeMillis() - start)
        _ <- EitherT(MicroblockAppender(checkpoint, history, blockchainUpdater, utx)(microBlock))
      } yield {
        BlockStats.mined(microBlock)
        log.trace(s"$microBlock has been mined for $account}")
        allChannels.broadcast(MicroBlockInv(account, microBlock.totalResBlockSig, microBlock.prevResBlockSig))
        Some(signedBlock)
      }).value.map {
        _.left.map { err =>
          log.trace(s"MicroBlock has NOT been mined for $account} because $err")
          err
        }
      }
    }
  }.flatten

  private def generateMicroBlockSequence(account: PrivateKeyAccount, accumulatedBlock: Block, delay: FiniteDuration): Task[Unit] = {
    debugState = State.Mining_Micros
    generateOneMicroBlockTask(account, accumulatedBlock).delayExecution(delay).flatMap {
      case Left(err) => Task {
        debugState = State.Mining_Blocks
        log.warn("Error mining MicroBlock: " + err.toString)
      }
      case Right(maybeNewTotal) => generateMicroBlockSequence(account, maybeNewTotal.getOrElse(accumulatedBlock), minerSettings.microBlockInterval)
    }
  }

  private def generateBlockTask(account: PrivateKeyAccount): Task[Unit] = {
    history.read { implicit l =>
      val height = history.height()
      val lastBlock = history.lastBlock.get
      for {
        _ <- checkAge(height, history.lastBlockTimestamp().get)
        balanceAndTs <- nextBlockGenerationTime(height, stateReader, blockchainSettings.functionalitySettings, lastBlock, account, featureProvider)
        (balance, ts) = balanceAndTs
        offset = calcOffset(timeService, ts, minerSettings.minimalBlockGenerationOffset)
      } yield (offset, balance)
    } match {
      case Right((offset, balance)) =>
        log.debug(s"Next attempt for acc=$account in $offset")
        nextBlockGenerationTimes += account.toAddress -> (System.currentTimeMillis() + offset.toMillis)
        generateOneBlockTask(account, balance)(offset).flatMap {
          case Right(block) =>
            BlockAppender(checkpoint, history, blockchainUpdater, timeService, stateReader, utx, settings.blockchainSettings, featureProvider)(block) map {
              case Left(err) => log.warn("Error mining Block: " + err.toString)
              case Right(Some(score)) =>
                log.debug(s"Forged and applied $block by ${account.address} with cumulative score $score")
                BlockStats.mined(block, history.height())
                allChannels.broadcast(BlockForged(block))
                scheduleMining()
                if (ngEnabled)
                  startMicroBlockMining(account, block)
              case Right(None) => log.warn("Newly created block has already been appended, should not happen")
            }
          case Left(err) =>
            log.debug(s"No block generated because $err, retrying")
            generateBlockTask(account)
        }
      case Left(err) =>
        log.debug(s"Not scheduling block mining because $err")
        debugState = State.Error
        Task.unit
    }
  }

  def scheduleMining(): Unit = {
    Miner.blockMiningStarted.increment()
    scheduledAttempts := CompositeCancelable.fromSet(
      wallet.privateKeyAccounts.map(generateBlockTask).map(_.runAsyncLogErr).toSet)
    microBlockAttempt := SerialCancelable()
    debugState = State.Mining_Blocks
  }

  private def startMicroBlockMining(account: PrivateKeyAccount, lastBlock: Block): Unit = {
    Miner.microMiningStarted.increment()
    microBlockAttempt := generateMicroBlockSequence(account, lastBlock, Duration.Zero).runAsyncLogErr
    log.trace(s"MicroBlock mining scheduled for $account")
  }

  override def state: MinerDebugInfo.State.Value = debugState
}

object Miner {
  val blockMiningStarted = Kamon.metrics.counter("block-mining-started")
  val microMiningStarted = Kamon.metrics.counter("micro-mining-started")

  val MaxTransactionsPerMicroblock: Int = 500
  val ClassicAmountOfTxsInBlock: Int = 100

  val Disabled = new Miner with MinerDebugInfo {
    override def scheduleMining(): Unit = ()

    override def collectNextBlockGenerationTimes: List[(Address, Long)] = List.empty

    override val state = MinerDebugInfo.State.Disabled
  }

  def calcOffset(timeService: Time, calculatedTimestamp: Long, minimalBlockGenerationOffset: FiniteDuration): FiniteDuration = {
    val calculatedGenerationTimestamp = (Math.ceil(calculatedTimestamp / 1000.0) * 1000).toLong
    val calculatedOffset = calculatedGenerationTimestamp - timeService.correctedTime()
    Math.max(minimalBlockGenerationOffset.toMillis, calculatedOffset).millis
  }
}
