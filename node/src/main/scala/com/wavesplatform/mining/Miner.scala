package com.wavesplatform.mining

import cats.data.EitherT
import cats.implicits._
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.block.Block._
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.{BlockStats, HistogramExt, Instrumented, _}
import com.wavesplatform.network._
import com.wavesplatform.settings.{FunctionalitySettings, WavesSettings}
import com.wavesplatform.state._
import com.wavesplatform.state.appender.{BlockAppender, MicroblockAppender}
import com.wavesplatform.transaction._
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import monix.eval.{MVar, Task}
import monix.execution.cancelables.{CompositeCancelable, SerialCancelable}
import monix.execution.schedulers.SchedulerService

import scala.concurrent.duration._

trait Miner {
  def scheduleMining(): Unit
}

trait MinerDebugInfo {
  def state: MinerDebugInfo.State
  def getNextBlockGenerationOffset(account: KeyPair): Either[String, FiniteDuration]
}

object MinerDebugInfo {
  sealed trait State
  case object MiningBlocks              extends State
  case object MiningMicroblocks         extends State
  case object Disabled                  extends State
  final case class Error(error: String) extends State
}

class MinerImpl(allChannels: ChannelGroup,
                blockchainUpdater: BlockchainUpdater with NG,
                settings: WavesSettings,
                timeService: Time,
                utx: UtxPoolImpl,
                wallet: Wallet,
                pos: PoSSelector,
                val minerScheduler: SchedulerService,
                val appenderScheduler: SchedulerService)
    extends Miner
    with MinerDebugInfo
    with ScorexLogging {

  import Miner._

  private implicit val s: SchedulerService = minerScheduler

  private lazy val minerSettings              = settings.minerSettings
  private lazy val minMicroBlockDurationMills = minerSettings.minMicroBlockAge.toMillis
  private lazy val blockchainSettings         = settings.blockchainSettings

  private val scheduledAttempts = SerialCancelable()
  private val microBlockAttempt = SerialCancelable()

  @volatile private var debugState: MinerDebugInfo.State = MinerDebugInfo.Disabled

  def getNextBlockGenerationOffset(account: KeyPair): Either[String, FiniteDuration] =
    this.nextBlockGenOffsetWithConditions(account)

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

  private def checkScript(account: KeyPair): Either[String, Unit] = {
    Either.cond(!blockchainUpdater.hasScript(account), (), s"Account(${account.toAddress}) is scripted and therefore not allowed to forge blocks")
  }

  private def ngEnabled: Boolean = blockchainUpdater.featureActivationHeight(BlockchainFeatures.NG.id).exists(blockchainUpdater.height > _ + 1)

  private def generateOneBlockTask(account: KeyPair)(delay: FiniteDuration): Task[Either[String, (MiningConstraints, Block, MiningConstraint)]] = {
    Task {
      forgeBlock(account)
    }.delayExecution(delay)
  }

  private def consensusData(height: Int,
                            account: KeyPair,
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
        blockchainUpdater.parentHeader(lastBlock, 2).map(_.timestamp),
        currentTime
      )
      .leftMap(_.toString)
  }

  private def forgeBlock(account: KeyPair): Either[String, (MiningConstraints, Block, MiningConstraint)] = {
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
    lazy val balance        = blockchainUpdater.generatingBalance(account.toAddress, refBlockID)

    metrics.blockBuildTimeStats.measureSuccessful(for {
      _ <- checkQuorumAvailable()
      validBlockDelay <- pos
        .getValidBlockDelay(height, account.publicKey, refBlockBT, balance)
        .leftMap(_.toString)
        .ensure(s"$currentTime: Block delay $blockDelay was NOT less than estimated delay")(_ < blockDelay)
      _ = log.debug(s"Forging with ${account.address}, Time $blockDelay > Estimated Time $validBlockDelay, balance $balance, prev block $refBlockID")
      _ = log.debug(s"Previous block ID $refBlockID at $height with target $refBlockBT")
      consensusData <- consensusData(height, account, lastBlock, refBlockBT, refBlockTS, balance, currentTime)
      estimators   = MiningConstraints(blockchainUpdater, height, Some(minerSettings))
      mdConstraint = MultiDimensionalMiningConstraint(estimators.total, estimators.keyBlock)
      (unconfirmed, updatedMdConstraint) = metrics.measureLog("packing unconfirmed transactions for block")(
        utx.packUnconfirmed(mdConstraint, settings.minerSettings.maxPackTime))
      _ = log.debug(s"Adding ${unconfirmed.size} unconfirmed transaction(s) to new block")
      block <- Block
        .buildAndSign(version.toByte, currentTime, refBlockID, consensusData, unconfirmed, account, blockFeatures(version))
        .leftMap(_.err)
    } yield (estimators, block, updatedMdConstraint.constraints.head))
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

  private def appendMicroBlock(microblock: MicroBlock): Task[ValidationError Either Unit] =
    MicroblockAppender(blockchainUpdater, utx, appenderScheduler, verify = false)(microblock)

  private def appendMicroBlockLoop(blockVar: MVar[Option[MicroBlock]]): Task[Unit] = {
    Task.tailRecM(blockVar) { bv =>
      bv.take
        .flatMap {
          case None => ().asRight.pure[Task]
          case Some(microBlock) =>
            appendMicroBlock(microBlock)
              .flatMap {
                case Left(err) =>
                  Task.delay {
                    debugState = MinerDebugInfo.Error(err.toString)
                    log.warn("Error mining MicroBlock: " + err.toString)

                    ().asRight
                  }
                case _ =>
                  bv.asLeft
                    .pure[Task]
                    .delayExecution(settings.minerSettings.microBlockInterval)
              }
        }
    }
  }

  case class MicroBlockMiningState(accumulatedBlock: Block, restTotalConstraint: MiningConstraint)

  def generateMicroBlocks(account: KeyPair,
                          accumulatedBlock: Block,
                          constraints: MiningConstraints,
                          restTotalConstraint: MiningConstraint): Task[Unit] = {

    for {
      blockVar <- MVar.empty[Option[MicroBlock]]
      generateTask = generateMicroBlockLoop(blockVar, account, accumulatedBlock, constraints, restTotalConstraint)
      appendTask   = appendMicroBlockLoop(blockVar)
      raceResult <- Task.racePair(generateTask, appendTask)
      _ <- raceResult match {
        case Left((_, appendFiber)) =>
          Task.delay {
            log.trace("Microblock mining stopped, stopping append fiber")
          } *> blockVar.put(none) *> appendFiber.cancel
        case Right((generateFiber, _)) =>
          Task.delay {
            log.trace("Microblock appending stopped, stopping generate fiber")
          } *> generateFiber.cancel
      }
    } yield ()
  }

  private def generateMicroBlockLoop(blockVar: MVar[Option[MicroBlock]],
                                     account: KeyPair,
                                     accumulatedBlock: Block,
                                     constraints: MiningConstraints,
                                     restTotalConstraint: MiningConstraint): Task[Unit] = {
    val initialMiningState = MicroBlockMiningState(accumulatedBlock, restTotalConstraint)

    Task.tailRecM(initialMiningState) { state =>
      Task
        .delay(generateMicroBlockTask(account, state.accumulatedBlock, constraints, state.restTotalConstraint))
        .flatMap {
          case Stop =>
            Task.delay {
              log.trace {
                if (state.restTotalConstraint.isFull) s"Stopping forging microBlocks, the block is full: ${state.restTotalConstraint}"
                else "Stopping forging microBlocks, because all transactions are too big"
              }.asRight
            }
          case Retry =>
            state
              .asLeft
              .pure[Task]

          case Delay(d) =>
            Task.delay {
              log.trace(s"Quorum not available (${allChannels
                .size()}/${minerSettings.quorum}), not forging microblock with ${account.address}, next attempt in ${d.toSeconds} seconds")
            } *> state.asLeft.pure[Task].delayExecution(d)

          case Error(e) =>
            Task {
              debugState = MinerDebugInfo.Error(e.toString)
              log.warn(s"Error mining MicroBlock for account: ${account.address}: " + e.toString)
              ().asRight
            }

          case Success(accumulated, microBlock, totalConstraint) => {
            val updState = MicroBlockMiningState(
              accumulated,
              totalConstraint
            ).asLeft

            blockVar.put(microBlock.some) as updState
          }
        }
    }
  }

  private def generateMicroBlockTask(account: KeyPair,
                                     accumulatedBlock: Block,
                                     constraints: MiningConstraints,
                                     restTotalConstraint: MiningConstraint): MicroblockMiningResult = {
    val pc = allChannels.size()

    if (pc < minerSettings.quorum) Delay(settings.minerSettings.noQuorumMiningDelay)
    else if (utx.size == 0) Retry
    else {
      val (unconfirmed, updatedTotalConstraint) = packTransactionsForMicroBlock(restTotalConstraint, constraints)

      if (unconfirmed.isEmpty) Stop
      else {
        log.trace(s"Accumulated ${unconfirmed.size} txs for microblock")

        forgeMicroBlock(account, accumulatedBlock, unconfirmed)
          .fold[MicroblockMiningResult](
            err => Error(err),
            blocks => Success(blocks._2, blocks._1, updatedTotalConstraint)
          )
      }
    }
  }

  def packTransactionsForMicroBlock(restTotalConstraint: MiningConstraint, constraints: MiningConstraints): (Seq[Transaction], MiningConstraint) =
    metrics.measureLog("packing unconfirmed transactions for microblock") {
      val mdConstraint                       = MultiDimensionalMiningConstraint(restTotalConstraint, constraints.micro)
      val (unconfirmed, updatedMdConstraint) = utx.packUnconfirmed(mdConstraint, settings.minerSettings.maxPackTime)
      (unconfirmed, updatedMdConstraint.constraints.head)
    }

  def forgeMicroBlock(account: KeyPair, accumulatedBlock: Block, unconfirmed: Seq[Transaction]): ValidationError Either (MicroBlock, Block) =
    for {
      signedBlock <- Block.buildAndSign(
        version = 3,
        timestamp = accumulatedBlock.timestamp,
        reference = accumulatedBlock.reference,
        consensusData = accumulatedBlock.consensusData,
        transactionData = accumulatedBlock.transactionData ++ unconfirmed,
        signer = account,
        featureVotes = accumulatedBlock.featureVotes
      )
      microBlock <- MicroBlock.buildAndSign(
        account,
        unconfirmed,
        accumulatedBlock.signerData.signature,
        signedBlock.signerData.signature
      )
    } yield (microBlock, signedBlock)

  private def nextBlockGenerationTime(fs: FunctionalitySettings, height: Int, block: Block, account: PublicKey): Either[String, Long] = {
    val balance = blockchainUpdater.generatingBalance(account.toAddress, block.uniqueId)

    if (blockchainUpdater.isMiningAllowed(height, balance)) {
      for {
        expectedTS <- pos
          .getValidBlockDelay(height, account, block.consensusData.baseTarget, balance)
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

  private def nextBlockGenOffsetWithConditions(account: KeyPair): Either[String, FiniteDuration] = {
    val height    = blockchainUpdater.height
    val lastBlock = blockchainUpdater.lastBlock.get
    for {
      _  <- checkAge(height, blockchainUpdater.lastBlockTimestamp.get) // lastBlock ?
      _  <- checkScript(account)
      ts <- nextBlockGenerationTime(blockchainSettings.functionalitySettings, height, lastBlock, account)
      calculatedOffset = ts - timeService.correctedTime()
      offset           = Math.max(calculatedOffset, minerSettings.minimalBlockGenerationOffset.toMillis).millis

    } yield offset
  }

  private def generateBlockTask(account: KeyPair): Task[Unit] = {
    {
      for {
        offset <- nextBlockGenOffsetWithConditions(account)
        quorumAvailable = checkQuorumAvailable().isRight
      } yield {
        if (quorumAvailable) offset
        else offset.max(settings.minerSettings.noQuorumMiningDelay)
      }
    } match {
      case Right(offset) =>
        log.debug(s"Next attempt for acc=$account in $offset")
        generateOneBlockTask(account)(offset).flatMap {
          case Right((estimators, block, totalConstraint)) =>
            BlockAppender(blockchainUpdater, timeService, utx, pos, appenderScheduler)(block)
              .asyncBoundary(minerScheduler)
              .map {
                case Left(err) => log.warn("Error mining Block: " + err.toString)
                case Right(Some(score)) =>
                  log.debug(s"Forged and applied $block by ${account.address} with cumulative score $score")
                  BlockStats.mined(block, blockchainUpdater.height)
                  allChannels.broadcast(BlockForged(block))
                  scheduleMining()
                  if (ngEnabled && !totalConstraint.isFull) startMicroBlockMining(account, block, estimators, totalConstraint)
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

  private[this] def startMicroBlockMining(account: KeyPair,
                                          lastBlock: Block,
                                          constraints: MiningConstraints,
                                          restTotalConstraint: MiningConstraint): Unit = {
    log.info(s"Start mining microblocks")
    Miner.microMiningStarted.increment()
    microBlockAttempt := generateMicroBlocks(account, lastBlock, constraints, restTotalConstraint).runAsyncLogErr //generateMicroBlockSequence(account, lastBlock, Duration.Zero, constraints, restTotalConstraint).runAsyncLogErr
    log.trace(s"MicroBlock mining scheduled for $account")
  }

  override def state: MinerDebugInfo.State = debugState

  private[this] object metrics {
    def measureLog[R](s: String)(f: => R): R = {
      val (result, time) = Instrumented.withTimeMillis(f)
      log.trace(s"$s took ${time}ms")
      result
    }

    val blockBuildTimeStats      = Kamon.timer("miner.pack-and-forge-block-time")
    val microBlockBuildTimeStats = Kamon.timer("miner.forge-microblock-time")
  }
}

object Miner {
  val blockMiningStarted = Kamon.counter("block-mining-started")
  val microMiningStarted = Kamon.counter("micro-mining-started")

  val MaxTransactionsPerMicroblock: Int = 500

  case object Disabled extends Miner with MinerDebugInfo {
    override def scheduleMining(): Unit                                                         = ()
    override def getNextBlockGenerationOffset(account: KeyPair): Either[String, FiniteDuration] = Left("Disabled")
    override val state                                                                          = MinerDebugInfo.Disabled
  }

  sealed trait MicroblockMiningResult
  case object Stop                                                                                        extends MicroblockMiningResult
  case object Retry                                                                                       extends MicroblockMiningResult
  final case class Delay(d: FiniteDuration)                                                               extends MicroblockMiningResult
  final case class Error(e: ValidationError)                                                              extends MicroblockMiningResult
  final case class Success(accumulated: Block, microBlock: MicroBlock, totalConstraint: MiningConstraint) extends MicroblockMiningResult

}
