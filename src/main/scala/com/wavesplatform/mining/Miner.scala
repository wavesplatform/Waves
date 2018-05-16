package com.wavesplatform.mining

import cats.data.EitherT
import com.wavesplatform.consensus.{GeneratingBalanceProvider, PoSCalculator}
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
import kamon.metric.instrument
import monix.eval.Task
import monix.execution.cancelables.{CompositeCancelable, SerialCancelable}
import monix.execution.schedulers.SchedulerService
import scorex.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import scorex.block.Block._
import scorex.block.{Block, MicroBlock}
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}
import scorex.wallet.Wallet

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
                pos: PoSCalculator,
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

  private val blockBuildTimeStats      = Kamon.metrics.histogram("pack-and-forge-block-time", instrument.Time.Milliseconds)
  private val microBlockBuildTimeStats = Kamon.metrics.histogram("forge-microblock-time", instrument.Time.Milliseconds)

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

  private def ngEnabled: Boolean = blockchainUpdater.featureActivationHeight(BlockchainFeatures.NG.id).exists(blockchainUpdater.height > _ + 1)

  private def generateOneBlockTask(account: PrivateKeyAccount, balance: Long)(
      delay: FiniteDuration): Task[Either[String, (MiningConstraints, Block, MiningConstraint)]] =
    Task {
      forgeBlock(account, balance)
    }.delayExecution(delay)

  private def forgeBlock(account: PrivateKeyAccount, balance: Long): Either[String, (MiningEstimators, Block, MiningConstraint)] = {
    // should take last block right at the time of mining since microblocks might have been added
    val height                    = blockchainUpdater.height
    val version                   = if (height <= blockchainSettings.functionalitySettings.blockVersion3AfterHeight) PlainBlockVersion else NgBlockVersion
    val lastBlock                 = blockchainUpdater.lastBlock.get
    val greatGrandParentTimestamp = blockchainUpdater.parent(lastBlock, 2).map(_.timestamp)
    val referencedBlockInfo       = blockchainUpdater.bestLastBlockInfo(System.currentTimeMillis() - minMicroBlockDurationMills).get
    val pc                        = allChannels.size()
    lazy val currentTime          = timeService.correctedTime()
    lazy val blockDelay           = currentTime - lastBlock.timestamp
    lazy val validBlockDelay = pos.validBlockDelay(
      referencedBlockInfo.consensus.generationSignature.arr,
      account.publicKey,
      referencedBlockInfo.consensus.baseTarget,
      balance
    )
    measureSuccessful(
      blockBuildTimeStats,
      for {
        _ <- Either.cond(pc >= minerSettings.quorum,
                         (),
                         s"Quorum not available ($pc/${minerSettings.quorum}, not forging block with ${account.address}")
        _ <- Either.cond(
          blockDelay > validBlockDelay,
          (),
          s"${System.currentTimeMillis()}: Block delay $blockDelay was NOT less than estimated delay $validBlockDelay, not forging block with ${account.address}"
        )
        _ = log.debug(
          s"Forging with ${account.address}, Time $blockDelay > Estimated Time $validBlockDelay, balance $balance, prev block ${referencedBlockInfo.blockId}")
        _ = log.debug(s"Previous block ID ${referencedBlockInfo.blockId} at $height with target ${referencedBlockInfo.consensus.baseTarget}")
        block <- {
          val avgBlockDelay = blockchainSettings.genesisSettings.averageBlockDelay
          val btg = pos.baseTarget(avgBlockDelay.toSeconds,
                                   height,
                                   referencedBlockInfo.consensus.baseTarget,
                                   referencedBlockInfo.timestamp,
                                   greatGrandParentTimestamp,
                                   currentTime)
          val gs            = pos.generatorSignature(referencedBlockInfo.consensus.generationSignature.arr, account.publicKey)
          val consensusData = NxtLikeConsensusBlockData(btg, ByteStr(gs))
          val sortInBlock   = blockchainUpdater.height <= blockchainSettings.functionalitySettings.dontRequireSortedTransactionsAfter

          val estimators                         = MiningConstraints(minerSettings, blockchainUpdater, height)
          val mdConstraint                       = MultiDimensionalMiningConstraint(estimators.total, estimators.keyBlock)
          val (unconfirmed, updatedMdConstraint) = utx.packUnconfirmed(mdConstraint, sortInBlock)

          val features =
            if (version <= 2) Set.empty[Short]
            else
              settings.featuresSettings.supported
                .filterNot(blockchainUpdater.approvedFeatures.keySet)
                .filter(BlockchainFeatures.implemented)
                .toSet

          log.debug(s"Adding ${unconfirmed.size} unconfirmed transaction(s) to new block")
          Block.buildAndSign(version.toByte, currentTime, referencedBlockInfo.blockId, consensusData, unconfirmed, account, features) match {
            case Left(e)  => Left(e.err)
            case Right(x) => Right((estimators, x, updatedMdConstraint.constraints.head))
          }
        }
      } yield block
    )
  }

  private def generateOneMicroBlockTask(account: PrivateKeyAccount,
                                        accumulatedBlock: Block,
                                        constraints: MiningConstraints,
                                        restTotalConstraint: MiningConstraint): Task[MicroblockMiningResult] = {
    log.trace(s"Generating microBlock for $account, constraints: $restTotalConstraint")
    val pc = allChannels.size()
    if (pc < minerSettings.quorum) {
      log.trace(s"Quorum not available ($pc/${minerSettings.quorum}, not forging microblock with ${account.address}")
      Task.now(Retry)
    } else if (utx.size == 0) {
      log.trace(s"Skipping microBlock because utx is empty")
      Task.now(Retry)
    } else {
      val (unconfirmed, updatedTotalConstraint) = measureLog("packing unconfirmed transactions for microblock") {
        val mdConstraint                       = MultiDimensionalMiningConstraint(restTotalConstraint, constraints.micro)
        val (unconfirmed, updatedMdConstraint) = utx.packUnconfirmed(mdConstraint, sortInBlock = false)
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
        case Retry => generateMicroBlockSequence(account, accumulatedBlock, minerSettings.microBlockInterval, constraints, restTotalConstraint)
        case Stop =>
          Task {
            debugState = MinerDebugInfo.MiningBlocks
            log.debug("MicroBlock mining completed, block is full")
          }
      }
  }

  private def nextBlockGenerationTime(fs: FunctionalitySettings,
                                      height: Int,
                                      block: Block,
                                      account: PublicKeyAccount): Either[String, (Long, Long)] = {
    val balance = GeneratingBalanceProvider.balance(blockchainUpdater, fs, height, account.toAddress)
    if (GeneratingBalanceProvider.isMiningAllowed(blockchainUpdater, height, balance)) {
      val cData        = block.consensusData
      val blockGS      = cData.generationSignature.arr
      val baseTarget   = cData.baseTarget
      val calculatedTs = pos.validBlockDelay(blockGS, account.publicKey, baseTarget, balance) + block.timestamp
      if (0 < calculatedTs && calculatedTs < Long.MaxValue) {
        Right((balance, calculatedTs))
      } else
        Left(s"Invalid next block generation time: $calculatedTs")
    } else Left(s"Balance $balance of ${account.address} is lower than required for generation")
  }

  private def generateBlockTask(account: PrivateKeyAccount): Task[Unit] = {
    {
      val height      = blockchainUpdater.height
      val blockForHit = (blockchainUpdater.blockAt(height - 100) orElse blockchainUpdater.lastBlock).get
      for {
        _ <- checkAge(height, blockchainUpdater.lastBlockTimestamp.get)
        _ <- Either.cond(blockchainUpdater.accountScript(account).isEmpty,
                         (),
                         s"Account(${account.toAddress}) is scripted and therefore not allowed to forge blocks")
        balanceAndTs <- nextBlockGenerationTime(blockchainSettings.functionalitySettings, height, blockForHit, account)
        (balance, ts) = balanceAndTs
        offset        = calcOffset(timeService, ts, minerSettings.minimalBlockGenerationOffset)
      } yield (offset, balance)
    } match {
      case Right((offset, balance)) =>
        log.debug(s"Next attempt for acc=$account in $offset")
        nextBlockGenerationTimes += account.toAddress -> (System.currentTimeMillis() + offset.toMillis)
        generateOneBlockTask(account, balance)(offset).flatMap {
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
    val nonScriptedAccounts = wallet.privateKeyAccounts.filter(acc => blockchainUpdater.accountScript(acc).isEmpty)
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
  val blockMiningStarted = Kamon.metrics.counter("block-mining-started")
  val microMiningStarted = Kamon.metrics.counter("micro-mining-started")

  val MaxTransactionsPerMicroblock: Int = 500

  val Disabled = new Miner with MinerDebugInfo {
    override def scheduleMining(): Unit = ()

    override def collectNextBlockGenerationTimes: List[(Address, Long)] = List.empty

    override val state = MinerDebugInfo.Disabled
  }

  def calcOffset(timeService: Time, calculatedTimestamp: Long, minimalBlockGenerationOffset: FiniteDuration): FiniteDuration = {
    val calculatedGenerationTimestamp = (Math.ceil(calculatedTimestamp / 1000.0) * 1000).toLong
    val calculatedOffset              = calculatedGenerationTimestamp - timeService.correctedTime()
    Math.max(minimalBlockGenerationOffset.toMillis, calculatedOffset).millis
  }

  sealed trait MicroblockMiningResult

  case object Stop                                                extends MicroblockMiningResult
  case object Retry                                               extends MicroblockMiningResult
  case class Error(e: ValidationError)                            extends MicroblockMiningResult
  case class Success(b: Block, totalConstraint: MiningConstraint) extends MicroblockMiningResult

}
