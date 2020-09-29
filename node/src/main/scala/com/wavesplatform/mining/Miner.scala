package com.wavesplatform.mining

import cats.implicits._
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block._
import com.wavesplatform.block.{Block, BlockHeader, MicroBlock}
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.{BlockStats, Instrumented, _}
import com.wavesplatform.mining.microblocks.MicroBlockMiner
import com.wavesplatform.network._
import com.wavesplatform.settings.MinerSettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.TxValidationError.BlockFromFuture
import com.wavesplatform.transaction._
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxPool.PackStrategy
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import monix.eval.Task
import monix.execution.cancelables.{CompositeCancelable, SerialCancelable}
import monix.execution.schedulers.SchedulerService
import monix.execution.{Cancelable, Scheduler}

import scala.concurrent.duration._

trait Miner {
  def scheduleMining(currentBlockchain: Blockchain): Unit
}

trait MinerDebugInfo {
  def state: MinerDebugInfo.State
  def nextBlockTime(account: KeyPair): Either[String, Long]
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
    blockchain: Blockchain with NG,
    minerSettings: MinerSettings,
    supportedFeatures: Set[Short],
    desiredReward: Option[Long],
    timeService: Time,
    utx: UtxPoolImpl,
    wallet: Wallet,
    minerScheduler: SchedulerService,
    appendBlock: Block => Task[Either[ValidationError, Option[BigInt]]],
    appendMicroblock: MicroBlock => Task[BlockId]
) extends Miner
    with MinerDebugInfo
    with ScorexLogging {

  private[this] implicit val implicitScheduler: SchedulerService = minerScheduler

  private[this] lazy val minMicroBlockDurationMills = minerSettings.minMicroBlockAge.toMillis
  private[this] lazy val blockchainSettings         = blockchain.settings

  private[this] val scheduledAttempts = SerialCancelable()
  private[this] val microBlockAttempt = SerialCancelable()

  @volatile
  private[this] var debugStateRef: MinerDebugInfo.State = MinerDebugInfo.Disabled

  private[this] val microBlockMiner: MicroBlockMiner = MicroBlockMiner(
    debugStateRef = _,
    allChannels,
    blockchain,
    utx,
    minerSettings,
    minerScheduler,
    appendMicroblock
  )

  def nextBlockTime(account: KeyPair): Either[String, Long] =
    Miner.nextBlockTime(blockchain, timeService, account, blockchain.lastBlockId)

  private def ngEnabled: Boolean = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).exists(blockchain.height > _ + 1)

  private def consensusData(height: Int, account: KeyPair, lastBlock: BlockHeader, blockTime: Long): Either[String, NxtLikeConsensusBlockData] =
    PoSSelector(blockchain)
      .consensusData(
        account,
        height,
        blockchainSettings.genesisSettings.averageBlockDelay,
        lastBlock.baseTarget,
        lastBlock.timestamp,
        blockchain.parentHeader(lastBlock, 2).map(_.timestamp),
        blockTime
      )
      .leftMap(_.toString)

  private def packTransactionsForKeyBlock(): (Seq[Transaction], MiningConstraint) = {
    val estimators = MiningConstraints(blockchain, blockchain.height, Some(minerSettings))
    if (blockchain.isFeatureActivated(BlockchainFeatures.NG)) (Seq.empty, estimators.total)
    else {
      val mdConstraint = MultiDimensionalMiningConstraint(estimators.total, estimators.keyBlock)
      val (maybeUnconfirmed, updatedMdConstraint) = Instrumented.logMeasure(log, "packing unconfirmed transactions for block")(
        utx.packUnconfirmed(mdConstraint, PackStrategy.Limit(minerSettings.microBlockInterval))
      )
      val unconfirmed = maybeUnconfirmed.getOrElse(Seq.empty)
      log.debug(s"Adding ${unconfirmed.size} unconfirmed transaction(s) to new block")
      (unconfirmed, updatedMdConstraint.constraints.head)
    }
  }

  private def bestMicroBlockReference =
    blockchain.bestLastBlockInfo(System.currentTimeMillis() - minMicroBlockDurationMills).get.blockId

  private[mining] def forgeBlock(account: KeyPair): Either[String, (Block, MiningConstraint)] = {
    log.debug(s"Forging for account ${account.toAddress}")
    // should take last block right at the time of mining since microblocks might have been added
    val height          = blockchain.height
    val version         = blockchain.nextBlockVersion
    val lastBlockHeader = blockchain.lastBlockHeader.get.header
    val reference       = bestMicroBlockReference

    metrics.blockBuildTimeStats.measureSuccessful(for {
      _         <- checkQuorumAvailable()
      blockTime <- Miner.nextBlockTime(blockchain, timeService, account, Some(reference))
      currentTime = timeService.correctedTime()
      _ <- Either.cond(
        blockTime <= currentTime + appender.MaxTimeDrift,
        log.debug(
          s"Forging with ${account.toAddress},prev block $reference at $height with target ${lastBlockHeader.baseTarget}"
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
      .ensureOr(channelCount => s"Quorum not available ($channelCount/${minerSettings.quorum}), not forging block.")(_ >= minerSettings.quorum)

  private def blockFeatures(version: Byte): Seq[Short] =
    if (version <= PlainBlockVersion) Nil
    else {
      val exclude = blockchain.approvedFeatures.keySet ++ blockchainSettings.functionalitySettings.preActivatedFeatures.keySet

      supportedFeatures
        .filterNot(exclude)
        .filter(BlockchainFeatures.implemented)
        .toSeq
        .sorted
    }

  private def blockRewardVote(version: Byte): Long =
    if (version < RewardBlockVersion) -1L
    else desiredReward.getOrElse(-1L)

  def mkBlock(parentId: Option[BlockId], account: KeyPair): Task[Either[String, (Block, MiningConstraint)]] = Task.defer {
    checkQuorumAvailable() match {
      case Left(err) =>
        log.debug(err)
        mkBlock(parentId, account).delayExecution(minerSettings.noQuorumMiningDelay)
      case Right(_) =>
        if (parentId.forall(blockchain.contains)) Task(forgeBlock(account))
        else mkBlock(parentId, account).delayExecution(100.millis)
    }
  }

  private[mining] def generateBlockTask(account: KeyPair, blockchain: Blockchain): Task[Unit] =
    mkBlock(blockchain.lastBlockId, account)
      .executeOn(minerScheduler)
      .flatMap {
        case Right((block, totalConstraint)) =>
          appendBlock(block)
            .executeOn(Scheduler.Implicits.global)
            .flatMap {
              case Left(BlockFromFuture(_)) => // Time was corrected, retry
                generateBlockTask(account, blockchain).delayExecution(appender.MaxTimeDrift.millis)

              case Left(err) =>
                log.error(s"Error appending block: $err")
                Task.raiseError(new RuntimeException(err.toString))

              case Right(Some(score)) =>
                log.info(s"Forged and applied block ${block.id()} with cumulative score $score")
                BlockStats.mined(block, blockchain.height)
                allChannels.broadcast(BlockForged(block))
                if (ngEnabled && !totalConstraint.isFull) startMicroBlockMining(account, block, totalConstraint)
                Task.unit

              case Right(None) =>
                log.warn("Newly created block has already been appended, should not happen")
                Task.unit
            }

        case Left(err) =>
          log.warn(s"Error mining block: $err")
          debugStateRef = MinerDebugInfo.Error(err)
          Task.unit
      }

  def scheduleMining(currentBlockchain: Blockchain): Unit = {
    Miner.blockMiningStarted.increment()

    scheduledAttempts := CompositeCancelable(
      wallet.privateKeyAccounts.filterNot(kp => currentBlockchain.hasAccountScript(kp.toAddress)).map { account =>
        Miner.nextBlockTime(currentBlockchain, timeService, account, currentBlockchain.lastBlockId) match {
          case Left(err) =>
            log.debug(s"Not scheduling block mining because $err")
            Cancelable.empty
          case Right(nextBlockTime) =>
            val offset = (nextBlockTime - timeService.correctedTime()).max(0L).millis
            log.debug(f"Next attempt for acc=${account.toAddress} in ${offset.toUnit(SECONDS)}%.3f seconds")
            minerScheduler.scheduleOnce(offset) {
              generateBlockTask(account, blockchain).runAsyncLogErr(minerScheduler)
            }
        }
      }: _*
    )
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
      .runAsyncLogErr
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
    override def scheduleMining(currentBlockchain: Blockchain): Unit          = {}
    override def nextBlockTime(account: KeyPair): Either[String, TxTimestamp] = Left("Miner is disabled")
    override val state: MinerDebugInfo.State                                  = MinerDebugInfo.Disabled
  }

  private[mining] def checkAge(parentHeight: Int, parentTimestamp: Long, timeService: Time, minerSettings: MinerSettings): Either[String, Unit] =
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

  private def checkScript(blockchain: Blockchain, account: KeyPair): Either[String, Unit] =
    Either.cond(
      !blockchain.hasAccountScript(account.toAddress),
      (),
      s"Account(${account.toAddress}) is scripted and therefore not allowed to forge blocks"
    )

  private[mining] def nextBlockTime(
      blockchain: Blockchain,
      timeService: Time,
      account: KeyPair,
      reference: Option[BlockId]
  ): Either[String, Long] =
    for {
      _ <- checkScript(blockchain, account)
      currentTime = timeService.correctedTime()
      generatingBalance <- Right(blockchain.generatingBalance(account.toAddress, reference))
        .ensureOr(effectiveBalance => s"generator's effective balance $effectiveBalance is less that required for generation")(
          blockchain.isEffectiveBalanceValid(blockchain.height, currentTime, _)
        )
      lastBlockHeader <- blockchain.lastBlockHeader.toRight("Blockchain is empty")
      validBlockDelay <- PoSSelector(blockchain)
        .getValidBlockDelay(blockchain.height, account, lastBlockHeader.header.baseTarget, generatingBalance)
        .leftMap(_.toString)
      blockTime = math.max(
        lastBlockHeader.header.timestamp + validBlockDelay,
        currentTime - blockchain.settings.functionalitySettings.maxTransactionTimeForwardOffset.toMillis
      )
    } yield blockTime
}
