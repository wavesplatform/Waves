package com.wavesplatform.mining.microblocks

import cats.syntax.applicativeError.*
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.metrics.*
import com.wavesplatform.mining.*
import com.wavesplatform.mining.microblocks.MicroBlockMinerImpl.*
import com.wavesplatform.network.{MicroBlockInv, *}
import com.wavesplatform.settings.MinerSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.appender.MicroblockAppender
import com.wavesplatform.transaction.{BlockchainUpdater, Transaction}
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.utx.UtxPool.PackStrategy
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable

import scala.concurrent.duration.*

class MicroBlockMinerImpl(
    setDebugState: MinerDebugInfo.State => Unit,
    allChannels: ChannelGroup,
    blockchainUpdater: BlockchainUpdater & Blockchain,
    utx: UtxPool,
    settings: MinerSettings,
    minerScheduler: SchedulerService,
    appenderScheduler: SchedulerService,
    transactionAdded: Observable[Unit],
    nextMicroBlockSize: Int => Int
) extends MicroBlockMiner
    with ScorexLogging {

  private val microBlockBuildTimeStats = Kamon.timer("miner.forge-microblock-time").withoutTags()

  def generateMicroBlockSequence(
      account: KeyPair,
      accumulatedBlock: Block,
      restTotalConstraint: MiningConstraint,
      lastMicroBlock: Long
  ): Task[Unit] =
    generateOneMicroBlockTask(account, accumulatedBlock, restTotalConstraint, lastMicroBlock)
      .flatMap {
        case res @ Success(newBlock, newConstraint) =>
          Task.defer(generateMicroBlockSequence(account, newBlock, newConstraint, res.nanoTime))
        case Retry =>
          Task
            .defer(generateMicroBlockSequence(account, accumulatedBlock, restTotalConstraint, lastMicroBlock))
            .delayExecution(1 second)
        case Stop =>
          setDebugState(MinerDebugInfo.MiningBlocks)
          Task(log.debug("MicroBlock mining completed, block is full"))
      }
      .recover { case e => log.error("Error mining microblock", e) }

  private[mining] def generateOneMicroBlockTask(
      account: KeyPair,
      accumulatedBlock: Block,
      restTotalConstraint: MiningConstraint,
      lastMicroBlock: Long
  ): Task[MicroBlockMiningResult] = {
    val packTask = Task.cancelable[(Option[Seq[Transaction]], MiningConstraint, Option[ByteStr])] { cb =>
      @volatile var cancelled = false
      minerScheduler.execute { () =>
        val mdConstraint = MultiDimensionalMiningConstraint(
          restTotalConstraint,
          OneDimensionalMiningConstraint(
            nextMicroBlockSize(settings.maxTransactionsInMicroBlock),
            TxEstimators.one,
            "MaxTxsInMicroBlock"
          )
        )
        val packStrategy =
          if (accumulatedBlock.transactionData.isEmpty) PackStrategy.Limit(settings.microBlockInterval)
          else PackStrategy.Estimate(settings.microBlockInterval)
        log.trace(s"Starting pack for ${accumulatedBlock.id()} with $packStrategy, initial constraint is $mdConstraint")
        val (unconfirmed, updatedMdConstraint, stateHash) =
          concurrent.blocking(
            Instrumented.logMeasure(log, "packing unconfirmed transactions for microblock")(
              utx.packUnconfirmed(
                mdConstraint,
                accumulatedBlock.header.stateHash,
                packStrategy,
                () => cancelled
              )
            )
          )
        log.trace(s"Finished pack for ${accumulatedBlock.id()}")
        val updatedTotalConstraint = updatedMdConstraint.head
        cb.onSuccess((unconfirmed, updatedTotalConstraint, stateHash))
      }
      Task.eval {
        cancelled = true
      }
    }

    packTask.flatMap {
      case (Some(unconfirmed), updatedTotalConstraint, stateHash) if unconfirmed.nonEmpty =>
        val delay = {
          val delay         = System.nanoTime() - lastMicroBlock
          val requiredDelay = settings.microBlockInterval.toNanos
          if (delay >= requiredDelay) Duration.Zero else (requiredDelay - delay).nanos
        }

        for {
          _ <- Task.now(if (delay > Duration.Zero) log.trace(s"Sleeping ${delay.toMillis} ms before applying microBlock"))
          _ <- Task.sleep(delay)
          r <-
            if (blockchainUpdater.lastBlockId.forall(_ == accumulatedBlock.id())) {
              log.trace(s"Generating microBlock for ${account.toAddress}, constraints: $updatedTotalConstraint")
              appendAndBroadcastMicroBlock(account, accumulatedBlock, unconfirmed, updatedTotalConstraint, stateHash)
            } else {
              log.trace(s"Stopping generating microBlock for ${account.toAddress}, new key block was appended")
              Task(Stop)
            }
        } yield r

      case (_, updatedTotalConstraint, _) =>
        if (updatedTotalConstraint.isFull) {
          log.trace(s"Stopping forging microBlocks, the block is full: $updatedTotalConstraint")
          Task.now(Stop)
        } else {
          log.trace("UTX is empty, waiting for new transactions")
          Task
            .race(
              transactionAdded.headL.map(_ => Retry),
              if (utx.size > 0) Task.now(Retry) else Task.never
            )
            .map(_.merge)
        }
    }
  }

  private def appendAndBroadcastMicroBlock(
      account: KeyPair,
      block: Block,
      transactions: Seq[Transaction],
      updatedTotalConstraint: MiningConstraint,
      stateHash: Option[BlockId]
  ): Task[MicroBlockMiningResult] =
    for {
      (signedBlock, microBlock) <- forgeBlocks(account, block, transactions, stateHash)
        .leftWiden[Throwable]
        .liftTo[Task]
      blockId <- appendMicroBlock(microBlock)
      _ = BlockStats.mined(microBlock, blockId)
      _ <- broadcastMicroBlock(account, microBlock, blockId)
    } yield
      if (updatedTotalConstraint.isFull) Stop
      else Success(signedBlock, updatedTotalConstraint)

  private def broadcastMicroBlock(account: KeyPair, microBlock: MicroBlock, blockId: BlockId): Task[Unit] =
    Task(if (allChannels != null) allChannels.broadcast(MicroBlockInv(account, blockId, microBlock.reference)))

  private def appendMicroBlock(microBlock: MicroBlock): Task[BlockId] =
    MicroblockAppender(blockchainUpdater, utx, appenderScheduler)(microBlock, None)
      .flatMap {
        case Left(err) => Task.raiseError(MicroBlockAppendError(microBlock, err))
        case Right(v)  => Task.now(v)
      }

  private def forgeBlocks(
      account: KeyPair,
      accumulatedBlock: Block,
      unconfirmed: Seq[Transaction],
      stateHash: Option[ByteStr]
  ): Either[MicroBlockMiningError, (Block, MicroBlock)] =
    microBlockBuildTimeStats.measureSuccessful {
      for {
        signedBlock <- Block
          .buildAndSign(
            version = blockchainUpdater.currentBlockVersion,
            timestamp = accumulatedBlock.header.timestamp,
            reference = accumulatedBlock.header.reference,
            baseTarget = accumulatedBlock.header.baseTarget,
            generationSignature = accumulatedBlock.header.generationSignature,
            txs = accumulatedBlock.transactionData ++ unconfirmed,
            signer = account,
            featureVotes = accumulatedBlock.header.featureVotes,
            rewardVote = accumulatedBlock.header.rewardVote,
            stateHash = stateHash,
            challengedHeader = None
          )
          .leftMap(BlockBuildError)
        microBlock <- MicroBlock
          .buildAndSign(signedBlock.header.version, account, unconfirmed, accumulatedBlock.id(), signedBlock.signature, stateHash)
          .leftMap(MicroBlockBuildError)
      } yield (signedBlock, microBlock)
    }
}

object MicroBlockMinerImpl {
  sealed trait MicroBlockMiningResult

  case object Stop  extends MicroBlockMiningResult
  case object Retry extends MicroBlockMiningResult
  final case class Success(b: Block, totalConstraint: MiningConstraint) extends MicroBlockMiningResult {
    val nanoTime: Long = System.nanoTime()
  }
}
