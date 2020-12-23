package com.wavesplatform.mining

import java.time.LocalTime

import scala.concurrent.duration._

import cats.implicits._
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, BlockHeader, SignedBlockHeader}
import com.wavesplatform.block.Block._
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.metrics.{BlockStats, Instrumented, _}
import com.wavesplatform.mining.microblocks.MicroBlockMiner
import com.wavesplatform.network._
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state._
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.TxValidationError.BlockFromFuture
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxPool.PackStrategy
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import monix.eval.Task
import monix.execution.cancelables.{CompositeCancelable, SerialCancelable}
import monix.execution.schedulers.SchedulerService

trait Miner {
  def scheduleMining(blockchain: Option[Blockchain] = None): Unit
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

class MinerImpl(
    allChannels: ChannelGroup,
    blockchainUpdater: Blockchain with BlockchainUpdater with NG,
    settings: WavesSettings,
    timeService: Time,
    utx: UtxPoolImpl,
    wallet: Wallet,
    pos: PoSSelector,
    val minerScheduler: SchedulerService,
    val appenderScheduler: SchedulerService
) extends Miner
    with MinerDebugInfo
    with ScorexLogging {

  private[this] val minerSettings              = settings.minerSettings
  private[this] val minMicroBlockDurationMills = minerSettings.minMicroBlockAge.toMillis
  private[this] val blockchainSettings         = settings.blockchainSettings

  private[this] val scheduledAttempts = SerialCancelable()
  private[this] val microBlockAttempt = SerialCancelable()

  @volatile
  private[this] var debugStateRef: MinerDebugInfo.State = MinerDebugInfo.Disabled

  private[this] val microBlockMiner: MicroBlockMiner = MicroBlockMiner(
    debugStateRef = _,
    allChannels,
    blockchainUpdater,
    utx,
    settings.minerSettings,
    minerScheduler,
    appenderScheduler,
    utx.priorityPool.nextMicroBlockSize
  )

  def getNextBlockGenerationOffset(account: KeyPair): Either[String, FiniteDuration] =
    this.nextBlockGenOffsetWithConditions(account, blockchainUpdater)

  private def checkAge(parentHeight: Int, parentTimestamp: Long): Either[String, Unit] =
    Either
      .cond(parentHeight == 1, (), (timeService.correctedTime() - parentTimestamp).millis)
      .left
      .flatMap(
        blockAge =>
          Either.cond(
            blockAge <= minerSettings.intervalAfterLastBlockThenGenerationIsAllowed,
            (),
            s"BlockChain is too old (last block timestamp is $parentTimestamp generated $blockAge ago)"
          )
      )

  private def checkScript(blockchain: Blockchain, account: KeyPair): Either[String, Unit] = {
    Either.cond(
      !blockchain.hasAccountScript(account.toAddress),
      (),
      s"Account(${account.toAddress}) is scripted and therefore not allowed to forge blocks"
    )
  }

  private def ngEnabled: Boolean = blockchainUpdater.featureActivationHeight(BlockchainFeatures.NG.id).exists(blockchainUpdater.height > _ + 1)

  private def consensusData(height: Int, account: KeyPair, lastBlock: BlockHeader, blockTime: Long): Either[String, NxtLikeConsensusBlockData] =
    pos
      .consensusData(
        account,
        height,
        blockchainSettings.genesisSettings.averageBlockDelay,
        lastBlock.baseTarget,
        lastBlock.timestamp,
        blockchainUpdater.parentHeader(lastBlock, 2).map(_.timestamp),
        blockTime
      )
      .leftMap(_.toString)

  private def packTransactionsForKeyBlock(): (Seq[Transaction], MiningConstraint) = {
    val estimators = MiningConstraints(blockchainUpdater, blockchainUpdater.height, Some(minerSettings))
    if (blockchainUpdater.isFeatureActivated(BlockchainFeatures.NG)) (Seq.empty, estimators.total)
    else {
      val mdConstraint = MultiDimensionalMiningConstraint(estimators.total, estimators.keyBlock)
      val (maybeUnconfirmed, updatedMdConstraint) = Instrumented.logMeasure(log, "packing unconfirmed transactions for block")(
        utx.packUnconfirmed(mdConstraint, PackStrategy.Limit(settings.minerSettings.microBlockInterval))
      )
      val unconfirmed = maybeUnconfirmed.getOrElse(Seq.empty)
      log.debug(s"Adding ${unconfirmed.size} unconfirmed transaction(s) to new block")
      (unconfirmed, updatedMdConstraint.constraints.head)
    }
  }

  private[mining] def forgeBlock(account: KeyPair): Either[String, (Block, MiningConstraint)] = {
    // should take last block right at the time of mining since microblocks might have been added
    val height          = blockchainUpdater.height
    val version         = blockchainUpdater.nextBlockVersion
    val lastBlockHeader = blockchainUpdater.lastBlockHeader.get.header
    val reference       = blockchainUpdater.bestLastBlockInfo(System.currentTimeMillis() - minMicroBlockDurationMills).get.blockId

    metrics.blockBuildTimeStats.measureSuccessful(for {
      _ <- checkQuorumAvailable()
      balance = blockchainUpdater.generatingBalance(account.toAddress, Some(reference))
      validBlockDelay <- pos
        .getValidBlockDelay(height, account, lastBlockHeader.baseTarget, balance)
        .leftMap(_.toString)
      currentTime = timeService.correctedTime()
      blockTime = math.max(
        lastBlockHeader.timestamp + validBlockDelay,
        currentTime - 1.minute.toMillis
      )
      _ <- Either.cond(
        blockTime <= currentTime + appender.MaxTimeDrift,
        log.debug(
          s"Forging with ${account.toAddress}, balance $balance, prev block $reference at $height with target ${lastBlockHeader.baseTarget}"
        ),
        s"Block time $blockTime is from the future: current time is $currentTime, MaxTimeDrift = ${appender.MaxTimeDrift}"
      )
      consensusData <- consensusData(height, account, lastBlockHeader, blockTime)
      (unconfirmed, totalConstraint) = packTransactionsForKeyBlock()
      block <- Block
        .buildAndSign(
          version,
          blockTime,
          reference,
          consensusData.baseTarget,
          consensusData.generationSignature,
          unconfirmed,
          account,
          blockFeatures(version),
          blockRewardVote(version)
        )
        .leftMap(_.err)
    } yield (block, totalConstraint))
  }

  private def checkQuorumAvailable(): Either[String, Int] =
    Right(allChannels.size())
      .ensureOr(chanCount => s"Quorum not available ($chanCount/${minerSettings.quorum}), not forging block.")(_ >= minerSettings.quorum)

  private def blockFeatures(version: Byte): Seq[Short] =
    if (version <= PlainBlockVersion) Nil
    else {
      val exclude = blockchainUpdater.approvedFeatures.keySet ++ settings.blockchainSettings.functionalitySettings.preActivatedFeatures.keySet

      settings.featuresSettings.supported
        .filterNot(exclude)
        .filter(BlockchainFeatures.implemented)
        .sorted
    }

  private def blockRewardVote(version: Byte): Long =
    if (version < RewardBlockVersion) -1L
    else settings.rewardsSettings.desired.getOrElse(-1L)

  private def nextBlockGenerationTime(blockchain: Blockchain, height: Int, block: SignedBlockHeader, account: KeyPair): Either[String, Long] = {
    val balance = blockchain.generatingBalance(account.toAddress, Some(block.id()))

    if (blockchain.isMiningAllowed(height, balance)) {
      val blockDelayE = pos.copy(blockchain = blockchain).getValidBlockDelay(height, account, block.header.baseTarget, balance)
      for {
        delay <- blockDelayE.leftMap(_.toString)
        expectedTS = delay + block.header.timestamp
        result <- Either.cond(
          0 < expectedTS && expectedTS < Long.MaxValue,
          expectedTS,
          s"Invalid next block generation time: $expectedTS"
        )
      } yield result
    } else Left(s"Balance $balance of ${account.toAddress} is lower than required for generation")
  }

  private def nextBlockGenOffsetWithConditions(account: KeyPair, blockchain: Blockchain): Either[String, FiniteDuration] = {
    val height    = blockchain.height
    val lastBlock = blockchain.lastBlockHeader.get
    for {
      _  <- checkAge(height, blockchain.lastBlockTimestamp.get) // lastBlock ?
      _  <- checkScript(blockchain, account)
      ts <- nextBlockGenerationTime(blockchain, height, lastBlock, account)
      calculatedOffset = ts - timeService.correctedTime()
      offset           = Math.max(calculatedOffset, minerSettings.minimalBlockGenerationOffset.toMillis).millis

    } yield offset
  }

  private[mining] def generateBlockTask(account: KeyPair, maybeBlockchain: Option[Blockchain]): Task[Unit] = {
    (for {
      offset <- nextBlockGenOffsetWithConditions(account, maybeBlockchain.getOrElse(blockchainUpdater))
      quorumAvailable = checkQuorumAvailable().isRight
    } yield {
      if (quorumAvailable) offset
      else offset.max(settings.minerSettings.noQuorumMiningDelay)
    }) match {
      case Right(offset) =>
        log.debug(
          f"Next attempt for acc=${account.toAddress} in ${offset.toUnit(SECONDS)}%.3f seconds (${LocalTime.now().plusNanos(offset.toNanos)})"
        )

        val waitBlockAppendedTask = maybeBlockchain match {
          case Some(value) =>
            def waitUntilBlockAppended(block: BlockId): Task[Unit] =
              if (blockchainUpdater.contains(block)) Task.unit
              else Task.defer(waitUntilBlockAppended(block)).delayExecution(1 seconds)

            waitUntilBlockAppended(value.lastBlockId.get)

          case None => Task.unit
        }

        def appendTask(block: Block, totalConstraint: MiningConstraint) =
          BlockAppender(blockchainUpdater, timeService, utx, pos, appenderScheduler)(block).flatMap {
            case Left(BlockFromFuture(_)) => // Time was corrected, retry
              generateBlockTask(account, None)

            case Left(err) =>
              Task.raiseError(new RuntimeException(err.toString))

            case Right(Some(score)) =>
              log.debug(s"Forged and applied $block with cumulative score $score")
              BlockStats.mined(block, blockchainUpdater.height)
              allChannels.broadcast(BlockForged(block))
              if (ngEnabled && !totalConstraint.isFull) startMicroBlockMining(account, block, totalConstraint)
              Task.unit

            case Right(None) =>
              Task.raiseError(new RuntimeException("Newly created block has already been appended, should not happen"))
          }.uncancelable

        for {
          elapsed <- waitBlockAppendedTask.timed.map(_._1)
          newOffset = (offset - elapsed).max(Duration.Zero)

          _      <- Task(microBlockAttempt := SerialCancelable()).delayExecution(newOffset)
          result <- Task(forgeBlock(account)).executeOn(minerScheduler)

          _ <- result match {
            case Right((block, totalConstraint)) =>
              appendTask(block, totalConstraint)

            case Left(err) =>
              log.debug(s"No block generated because $err, retrying")
              generateBlockTask(account, None)
          }
        } yield ()

      case Left(err) =>
        log.debug(s"Not scheduling block mining because $err")
        debugStateRef = MinerDebugInfo.Error(err)
        Task.unit
    }
  }

  def scheduleMining(tempBlockchain: Option[Blockchain]): Unit = {
    Miner.blockMiningStarted.increment()

    val nonScriptedAccounts = wallet.privateKeyAccounts.filterNot(kp => tempBlockchain.getOrElse(blockchainUpdater).hasAccountScript(kp.toAddress))
    scheduledAttempts := CompositeCancelable.fromSet(nonScriptedAccounts.map { account =>
      generateBlockTask(account, tempBlockchain)
        .onErrorHandle(err => log.warn(s"Error mining Block: $err"))
        .runAsyncLogErr(appenderScheduler)
    }.toSet)
    microBlockAttempt := SerialCancelable()

    debugStateRef = MinerDebugInfo.MiningBlocks
  }

  private[this] def startMicroBlockMining(
      account: KeyPair,
      lastBlock: Block,
      restTotalConstraint: MiningConstraint
  ): Unit = {
    Miner.microMiningStarted.increment()
    microBlockAttempt := microBlockMiner
      .generateMicroBlockSequence(account, lastBlock, restTotalConstraint, 0)
      .runAsyncLogErr(minerScheduler)
    log.trace(s"MicroBlock mining scheduled for acc=${account.toAddress}")
  }

  override def state: MinerDebugInfo.State = debugStateRef

  //noinspection TypeAnnotation,ScalaStyle
  private[this] object metrics {
    val blockBuildTimeStats = Kamon.timer("miner.pack-and-forge-block-time").withoutTags()
  }
}

object Miner {
  private[mining] val blockMiningStarted = Kamon.counter("block-mining-started").withoutTags()
  private[mining] val microMiningStarted = Kamon.counter("micro-mining-started").withoutTags()

  val MaxTransactionsPerMicroblock: Int = 500

  case object Disabled extends Miner with MinerDebugInfo {
    override def scheduleMining(blockchain: Option[Blockchain]): Unit                           = ()
    override def getNextBlockGenerationOffset(account: KeyPair): Either[String, FiniteDuration] = Left("Disabled")
    override val state: MinerDebugInfo.State                                                    = MinerDebugInfo.Disabled
  }
}
