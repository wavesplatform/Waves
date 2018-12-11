package com.wavesplatform.mining

import cats.data.EitherT
import cats.implicits._
import com.wavesplatform.consensus.{GeneratingBalanceProvider, PoSSelector}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.metrics.{BlockStats, HistogramExt, Instrumented}
import com.wavesplatform.network._
import com.wavesplatform.settings.{FunctionalitySettings, WavesSettings}
import com.wavesplatform.state._
import com.wavesplatform.state.appender.{BlockAppender, MicroblockAppender}
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import kamon.metric.MeasurementUnit
import monix.eval.Task
import monix.execution.cancelables.{CompositeCancelable, SerialCancelable}
import monix.execution.schedulers.SchedulerService
import com.wavesplatform.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.block.Block._
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.transaction._
import com.wavesplatform.wallet.Wallet

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.Await
import scala.concurrent.duration._

trait Miner {
  def scheduleMining(): Unit
}

trait MinerDebugInfo {
  def state: MinerDebugInfo.State

  def collectNextBlockGenerationTimes: List[(Address, Long)]
}

object MinerDebugInfo {

  sealed trait State

  case object MiningBlocks extends State

  case object MiningMicroblocks extends State

  case object Disabled extends State

  case class Error(error: String) extends State

}

class MinerImpl(allChannels: ChannelGroup,
                blockchainUpdater: BlockchainUpdater with NG,
                checkpoint: CheckpointService,
                settings: WavesSettings,
                timeService: Time,
                utx: UtxPool,
                wallet: Wallet,
                pos: PoSSelector,
                val minerScheduler: SchedulerService,
                val appenderScheduler: SchedulerService)
    extends Miner
    with MinerDebugInfo
    with ScorexLogging
    with Instrumented {

  import Miner._

  private implicit val s: SchedulerService = minerScheduler

  private lazy val minerSettings              = settings.minerSettings
  private lazy val minMicroBlockDurationMills = minerSettings.minMicroBlockAge.toMillis
  private lazy val blockchainSettings         = settings.blockchainSettings

  private val scheduledAttempts = SerialCancelable()
  private val microBlockAttempt = SerialCancelable()

  private val blockBuildTimeStats      = Kamon.histogram("pack-and-forge-block-time", MeasurementUnit.time.milliseconds)
  private val microBlockBuildTimeStats = Kamon.histogram("forge-microblock-time", MeasurementUnit.time.milliseconds)

  private val nextBlockGenerationTimes: MMap[Address, Long] = MMap.empty

  @volatile private var debugState: MinerDebugInfo.State = MinerDebugInfo.Disabled

  def collectNextBlockGenerationTimes: List[(Address, Long)] = Await.result(Task.now(nextBlockGenerationTimes.toList).runAsyncLogErr, Duration.Inf)

  private def checkAge(parentHeight: Int, parentTimestamp: Long): Either[String, Unit] =
    Either
      .cond(parentHeight == 1, (), (timeService.correctedTime() - parentTimestamp).millis)
      .left
      .flatMap(blockAge =>
        Either.cond(
          blockAge <= minerSettings.intervalAfterLastBlockThenGenerationIsAllowed,
          (),
          s"BlockChain is too old (last block timestamp is $parentTimestamp generated $blockAge ago)"
      ))

  private def checkScript(account: PrivateKeyAccount): Either[String, Unit] = {
    Either.cond(!blockchainUpdater.hasScript(account), (), s"Account(${account.toAddress}) is scripted and therefore not allowed to forge blocks")
  }

  private def ngEnabled: Boolean = blockchainUpdater.featureActivationHeight(BlockchainFeatures.NG.id).exists(blockchainUpdater.height > _ + 1)

  private def generateOneBlockTask(account: PrivateKeyAccount)(
      delay: FiniteDuration): Task[Either[String, (MiningConstraints, Block, MiningConstraint)]] = {
    Task {
      forgeBlock(account)
    }.delayExecution(delay)
  }

  private def consensusData(height: Int,
                            account: PrivateKeyAccount,
                            lastBlock: Block,
                            refBlockBT: Long,
                            refBlockTS: Long,
                            balance: Long,
                            currentTime: Long): Either[String, NxtLikeConsensusBlockData] = {
    pos
      .consensusData(
        account.publicKey,
        height,
        blockchainSettings.genesisSettings.averageBlockDelay,
        refBlockBT,
        refBlockTS,
        blockchainUpdater.parent(lastBlock, 2).map(_.timestamp),
        currentTime
      )
      .leftMap(_.toString)
  }

  private def forgeBlock(account: PrivateKeyAccount): Either[String, (MiningConstraints, Block, MiningConstraint)] = {
    // should take last block right at the time of mining since microblocks might have been added
    val height              = blockchainUpdater.height
    val version             = if (height <= blockchainSettings.functionalitySettings.blockVersion3AfterHeight) PlainBlockVersion else NgBlockVersion
    val lastBlock           = blockchainUpdater.lastBlock.get
    val referencedBlockInfo = blockchainUpdater.bestLastBlockInfo(System.currentTimeMillis() - minMicroBlockDurationMills).get
    val refBlockBT          = referencedBlockInfo.consensus.baseTarget
    val refBlockTS          = referencedBlockInfo.timestamp
    val refBlockID          = referencedBlockInfo.blockId
    lazy val currentTime    = timeService.correctedTime()
    lazy val blockDelay     = currentTime - lastBlock.timestamp
    lazy val balance        = GeneratingBalanceProvider.balance(blockchainUpdater, blockchainSettings.functionalitySettings, height, account.toAddress)

    measureSuccessful(
      blockBuildTimeStats,
      for {
        _ <- checkQuorumAvailable()
        validBlockDelay <- pos
          .getValidBlockDelay(height, account.publicKey, refBlockBT, balance)
          .leftMap(_.toString)
          .ensure(s"$currentTime: Block delay $blockDelay was NOT less than estimated delay")(_ < blockDelay)
        _ = log.debug(
          s"Forging with ${account.address}, Time $blockDelay > Estimated Time $validBlockDelay, balance $balance, prev block $refBlockID")
        _ = log.debug(s"Previous block ID $refBlockID at $height with target $refBlockBT")
        consensusData <- consensusData(height, account, lastBlock, refBlockBT, refBlockTS, balance, currentTime)
        estimators                         = MiningConstraints(blockchainUpdater, height, Some(minerSettings))
        mdConstraint                       = MultiDimensionalMiningConstraint(estimators.total, estimators.keyBlock)
        (unconfirmed, updatedMdConstraint) = utx.packUnconfirmed(mdConstraint)
        _                                  = log.debug(s"Adding ${unconfirmed.size} unconfirmed transaction(s) to new block")
        block <- Block
          .buildAndSign(version.toByte, currentTime, refBlockID, consensusData, unconfirmed, account, blockFeatures(version))
          .leftMap(_.err)
      } yield (estimators, block, updatedMdConstraint.constraints.head)
    )
  }

  private def checkQuorumAvailable(): Either[String, Unit] = {
    val chanCount = allChannels.size()
    Either.cond(chanCount >= minerSettings.quorum, (), s"Quorum not available ($chanCount/${minerSettings.quorum}), not forging block.")
  }

  private def blockFeatures(version: Byte): Set[Short] = {
    if (version <= 2) Set.empty[Short]
    else
      settings.featuresSettings.supported
        .filterNot(blockchainUpdater.approvedFeatures.keySet)
        .filter(BlockchainFeatures.implemented)
        .toSet
  }

  private def generateOneMicroBlockTask(account: PrivateKeyAccount,
                                        accumulatedBlock: Block,
                                        constraints: MiningConstraints,
                                        restTotalConstraint: MiningConstraint): Task[MicroblockMiningResult] = {
    log.trace(s"Generating microBlock for $account, constraints: $restTotalConstraint")
    val pc = allChannels.size()
    if (pc < minerSettings.quorum) {
      log.trace(s"Quorum not available ($pc/${minerSettings.quorum}), not forging microblock with ${account.address}, next attempt in 5 seconds")
      Task.now(Delay(settings.minerSettings.noQuorumMiningDelay))
    } else if (utx.size == 0) {
      log.trace(s"Skipping microBlock because utx is empty")
      Task.now(Retry)
    } else {
      val (unconfirmed, updatedTotalConstraint) = measureLog("packing unconfirmed transactions for microblock") {
        val mdConstraint                       = MultiDimensionalMiningConstraint(restTotalConstraint, constraints.micro)
        val (unconfirmed, updatedMdConstraint) = utx.packUnconfirmed(mdConstraint)
        (unconfirmed, updatedMdConstraint.constraints.head)
      }

      if (unconfirmed.isEmpty) {
        log.trace {
          if (updatedTotalConstraint.isEmpty) s"Stopping forging microBlocks, the block is full: $updatedTotalConstraint"
          else "Stopping forging microBlocks, because all transactions are too big"
        }
        Task.now(Stop)
      } else {
        log.trace(s"Accumulated ${unconfirmed.size} txs for microblock")
        val start = System.currentTimeMillis()
        (for {
          signedBlock <- EitherT.fromEither[Task](
            Block.buildAndSign(
              version = 3,
              timestamp = accumulatedBlock.timestamp,
              reference = accumulatedBlock.reference,
              consensusData = accumulatedBlock.consensusData,
              transactionData = accumulatedBlock.transactionData ++ unconfirmed,
              signer = account,
              featureVotes = accumulatedBlock.featureVotes
            ))
          microBlock <- EitherT.fromEither[Task](
            MicroBlock.buildAndSign(account, unconfirmed, accumulatedBlock.signerData.signature, signedBlock.signerData.signature))
          _ = microBlockBuildTimeStats.safeRecord(System.currentTimeMillis() - start)
          _ <- EitherT(MicroblockAppender(checkpoint, blockchainUpdater, utx, appenderScheduler)(microBlock))
        } yield (microBlock, signedBlock)).value map {
          case Left(err) => Error(err)
          case Right((microBlock, signedBlock)) =>
            BlockStats.mined(microBlock)
            allChannels.broadcast(MicroBlockInv(account, microBlock.totalResBlockSig, microBlock.prevResBlockSig))
            if (updatedTotalConstraint.isEmpty) {
              log.trace(s"$microBlock has been mined for $account. Stop forging microBlocks, the block is full: $updatedTotalConstraint")
              Stop
            } else {
              log.trace(s"$microBlock has been mined for $account")
              Success(signedBlock, updatedTotalConstraint)
            }
        }
      }
    }
  }

  private def generateMicroBlockSequence(account: PrivateKeyAccount,
                                         accumulatedBlock: Block,
                                         delay: FiniteDuration,
                                         constraints: MiningConstraints,
                                         restTotalConstraint: MiningConstraint): Task[Unit] = {
    debugState = MinerDebugInfo.MiningMicroblocks
    log.info(s"Generate MicroBlock sequence, delay = $delay")
    generateOneMicroBlockTask(account, accumulatedBlock, constraints, restTotalConstraint)
      .asyncBoundary(minerScheduler)
      .delayExecution(delay)
      .flatMap {
        case Error(e) =>
          Task {
            debugState = MinerDebugInfo.Error(e.toString)
            log.warn("Error mining MicroBlock: " + e.toString)
          }
        case Success(newTotal, updatedTotalConstraint) =>
          generateMicroBlockSequence(account, newTotal, minerSettings.microBlockInterval, constraints, updatedTotalConstraint)
        case Delay(d) =>
          generateMicroBlockSequence(account, accumulatedBlock, minerSettings.microBlockInterval, constraints, restTotalConstraint)
            .delayExecution(d)
        case Retry => generateMicroBlockSequence(account, accumulatedBlock, minerSettings.microBlockInterval, constraints, restTotalConstraint)
        case Stop =>
          Task {
            debugState = MinerDebugInfo.MiningBlocks
            log.debug("MicroBlock mining completed, block is full")
          }
      }
  }

  private def nextBlockGenerationTime(fs: FunctionalitySettings, height: Int, block: Block, account: PublicKeyAccount): Either[String, Long] = {
    val balance = GeneratingBalanceProvider.balance(blockchainUpdater, fs, height, account.toAddress)

    if (GeneratingBalanceProvider.isMiningAllowed(blockchainUpdater, height, balance)) {
      for {
        expectedTS <- pos
          .getValidBlockDelay(height, account.publicKey, block.consensusData.baseTarget, balance)
          .map(_ + block.timestamp)
          .leftMap(_.toString)
        result <- Either.cond(
          0 < expectedTS && expectedTS < Long.MaxValue,
          expectedTS,
          s"Invalid next block generation time: $expectedTS"
        )
      } yield result
    } else Left(s"Balance $balance of ${account.address} is lower than required for generation")
  }

  private def generateBlockTask(account: PrivateKeyAccount): Task[Unit] = {
    {
      val height    = blockchainUpdater.height
      val lastBlock = blockchainUpdater.lastBlock.get
      for {
        _  <- checkAge(height, blockchainUpdater.lastBlockTimestamp.get)
        _  <- checkScript(account)
        ts <- nextBlockGenerationTime(blockchainSettings.functionalitySettings, height, lastBlock, account)
        calculatedOffset = ts - timeService.correctedTime()
        offset           = Math.max(calculatedOffset, minerSettings.minimalBlockGenerationOffset.toMillis).millis
        quorumAvailable  = checkQuorumAvailable().isRight
      } yield {
        if (quorumAvailable) offset
        else offset.max(settings.minerSettings.noQuorumMiningDelay)
      }
    } match {
      case Right(offset) =>
        log.debug(s"Next attempt for acc=$account in $offset")
        nextBlockGenerationTimes += account.toAddress -> (System.currentTimeMillis() + offset.toMillis)
        generateOneBlockTask(account)(offset).flatMap {
          case Right((estimators, block, totalConstraint)) =>
            BlockAppender(checkpoint, blockchainUpdater, timeService, utx, pos, settings, appenderScheduler)(block)
              .asyncBoundary(minerScheduler)
              .map {
                case Left(err) => log.warn("Error mining Block: " + err.toString)
                case Right(Some(score)) =>
                  log.debug(s"Forged and applied $block by ${account.address} with cumulative score $score")
                  BlockStats.mined(block, blockchainUpdater.height)
                  allChannels.broadcast(BlockForged(block))
                  scheduleMining()
                  if (ngEnabled && !totalConstraint.isEmpty) startMicroBlockMining(account, block, estimators, totalConstraint)
                case Right(None) => log.warn("Newly created block has already been appended, should not happen")
              }

          case Left(err) =>
            log.debug(s"No block generated because $err, retrying")
            generateBlockTask(account)
        }

      case Left(err) =>
        log.debug(s"Not scheduling block mining because $err")
        debugState = MinerDebugInfo.Error(err)
        Task.unit
    }
  }

  def scheduleMining(): Unit = {
    Miner.blockMiningStarted.increment()
    val nonScriptedAccounts = wallet.privateKeyAccounts.filterNot(blockchainUpdater.hasScript(_))
    scheduledAttempts := CompositeCancelable.fromSet(nonScriptedAccounts.map(generateBlockTask).map(_.runAsyncLogErr).toSet)
    microBlockAttempt := SerialCancelable()
    debugState = MinerDebugInfo.MiningBlocks
  }

  private def startMicroBlockMining(account: PrivateKeyAccount,
                                    lastBlock: Block,
                                    constraints: MiningConstraints,
                                    restTotalConstraint: MiningConstraint): Unit = {
    log.info(s"Start mining microblocks")
    Miner.microMiningStarted.increment()
    microBlockAttempt := generateMicroBlockSequence(account, lastBlock, Duration.Zero, constraints, restTotalConstraint).runAsyncLogErr
    log.trace(s"MicroBlock mining scheduled for $account")
  }

  override def state: MinerDebugInfo.State = debugState
}

object Miner {
  val blockMiningStarted = Kamon.counter("block-mining-started")
  val microMiningStarted = Kamon.counter("micro-mining-started")

  val MaxTransactionsPerMicroblock: Int = 500

  val Disabled = new Miner with MinerDebugInfo {
    override def scheduleMining(): Unit = ()

    override def collectNextBlockGenerationTimes: List[(Address, Long)] = List.empty

    override val state = MinerDebugInfo.Disabled
  }

  sealed trait MicroblockMiningResult

  case object Stop                                                extends MicroblockMiningResult
  case object Retry                                               extends MicroblockMiningResult
  case class Delay(d: FiniteDuration)                             extends MicroblockMiningResult
  case class Error(e: ValidationError)                            extends MicroblockMiningResult
  case class Success(b: Block, totalConstraint: MiningConstraint) extends MicroblockMiningResult

}
