package com.wavesplatform.mining.microblocks

import cats.effect.concurrent.Ref
import cats.implicits._
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.metrics._
import com.wavesplatform.mining.microblocks.MicroBlockMinerImpl._
import com.wavesplatform.mining.{MinerDebugInfo, MiningConstraint, MiningConstraints, MultiDimensionalMiningConstraint}
import com.wavesplatform.network.{MicroBlockInv, _}
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

import scala.concurrent.duration._

class MicroBlockMinerImpl(
    debugState: Ref[Task, MinerDebugInfo.State],
    allChannels: ChannelGroup,
    blockchainUpdater: BlockchainUpdater with Blockchain,
    utx: UtxPool,
    settings: MinerSettings,
    minerScheduler: SchedulerService,
    appenderScheduler: SchedulerService
) extends MicroBlockMiner
    with ScorexLogging {

  val microBlockBuildTimeStats = Kamon.timer("miner.forge-microblock-time")

  def generateMicroBlockSequence(
      account: KeyPair,
      accumulatedBlock: Block,
      constraints: MiningConstraints,
      restTotalConstraint: MiningConstraint
  ): Task[Unit] = {
    generateOneMicroBlockTask(account, accumulatedBlock, constraints, restTotalConstraint)
      .flatMap {
        case Success(newBlock, newConstraint) =>
          Task.defer(generateMicroBlockSequence(account, newBlock, constraints, newConstraint))
        case Retry =>
          Task
            .defer(generateMicroBlockSequence(account, accumulatedBlock, constraints, restTotalConstraint))
            .delayExecution(1 second)
        case Stop =>
          debugState
            .set(MinerDebugInfo.MiningBlocks) >>
            Task(log.debug("MicroBlock mining completed, block is full"))
      }
      .recover { case e => log.error("Error mining microblock", e) }
  }

  private def generateOneMicroBlockTask(
      account: KeyPair,
      accumulatedBlock: Block,
      constraints: MiningConstraints,
      restTotalConstraint: MiningConstraint
  ): Task[MicroBlockMiningResult] = {
    val packTask = Task.cancelable[(Option[Seq[Transaction]], MiningConstraint)] { cb =>
      @volatile var cancelled = false
      minerScheduler.execute { () =>
        val mdConstraint = MultiDimensionalMiningConstraint(restTotalConstraint, constraints.micro)
        val packStrategy =
          if (accumulatedBlock.transactionData.isEmpty) PackStrategy.Limit(settings.microBlockInterval)
          else PackStrategy.Estimate(settings.microBlockInterval)
        log.info(s"Starting pack for ${accumulatedBlock.id()} with $packStrategy, initial constraint is $mdConstraint")
        val (unconfirmed, updatedMdConstraint) =
          concurrent.blocking(
            Instrumented.logMeasure(log, "packing unconfirmed transactions for microblock")(
              utx.packUnconfirmed(
                mdConstraint,
                packStrategy,
                () => cancelled
              )
            )
          )
        log.info("Finished pack")
        val updatedTotalConstraint = updatedMdConstraint.constraints.head
        cb.onSuccess(unconfirmed -> updatedTotalConstraint)
      }
      Task.eval {
        log.trace("Cancelling execution")
        cancelled = true
      }
    }

    packTask.flatMap {
      case (Some(unconfirmed), updatedTotalConstraint) if unconfirmed.nonEmpty =>
        log.trace(s"Generating microBlock for $account, constraints: $updatedTotalConstraint")

        for {
          blocks <- forgeBlocks(account, accumulatedBlock, unconfirmed)
            .leftWiden[Throwable]
            .liftTo[Task]
          (signedBlock, microBlock) = blocks
          blockId <- appendMicroBlock(microBlock)
          _       <- broadcastMicroBlock(account, microBlock, blockId)
        } yield {
          if (updatedTotalConstraint.isFull) Stop
          else Success(signedBlock, updatedTotalConstraint)
        }

      case (_, updatedTotalConstraint) =>
        if (updatedTotalConstraint.isFull) {
          log.trace(s"Stopping forging microBlocks, the block is full: $updatedTotalConstraint")
          Task.now(Stop)
        } else {
          log.trace("UTX is empty, retrying")
          Task.now(Retry)
        }
    }
  }

  private def broadcastMicroBlock(account: KeyPair, microBlock: MicroBlock, blockId: BlockId): Task[Unit] =
    Task(if (allChannels != null) allChannels.broadcast(MicroBlockInv(account, blockId, microBlock.reference)))

  private def appendMicroBlock(microBlock: MicroBlock): Task[BlockId] =
    MicroblockAppender(blockchainUpdater, utx, appenderScheduler)(microBlock)
      .flatMap {
        case Left(err) => Task.raiseError(MicroBlockAppendError(microBlock, err))
        case Right(v)  => Task.now(v)
      }

  private def forgeBlocks(
      account: KeyPair,
      accumulatedBlock: Block,
      unconfirmed: Seq[Transaction]
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
            rewardVote = accumulatedBlock.header.rewardVote
          )
          .leftMap(BlockBuildError)
        microBlock <- MicroBlock
          .buildAndSign(signedBlock.header.version, account, unconfirmed, accumulatedBlock.id(), signedBlock.signature)
          .leftMap(MicroBlockBuildError)
        _ = BlockStats.mined(microBlock)
      } yield (signedBlock, microBlock)
    }
}

object MicroBlockMinerImpl {
  sealed trait MicroBlockMiningResult

  case object Stop                                                      extends MicroBlockMiningResult
  case object Retry                                                     extends MicroBlockMiningResult
  final case class Success(b: Block, totalConstraint: MiningConstraint) extends MicroBlockMiningResult
}
