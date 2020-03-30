package com.wavesplatform.mining.microblocks

import cats.effect.concurrent.Ref
import cats.implicits._
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics._
import com.wavesplatform.mining.microblocks.MicroBlockMinerImpl._
import com.wavesplatform.mining.{MinerDebugInfo, MiningConstraint, MiningConstraints, MultiDimensionalMiningConstraint}
import com.wavesplatform.network.{MicroBlockInv, _}
import com.wavesplatform.settings.MinerSettings
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import kamon.metric.TimerMetric
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._

class MicroBlockMinerImpl(
    debugState: Ref[Task, MinerDebugInfo.State],
    allChannels: ChannelGroup,
    appendMicroBlockTask: MicroBlock => Task[Either[ValidationError, BlockId]],
    utx: UtxPool,
    settings: MinerSettings,
    minerScheduler: Scheduler
) extends MicroBlockMiner
    with ScorexLogging {

  val microBlockBuildTimeStats: TimerMetric = Kamon.timer("miner.forge-microblock-time")

  def generateMicroBlockSequence(
      account: KeyPair,
      accumulatedBlock: Block,
      delay: FiniteDuration,
      constraints: MiningConstraints,
      restTotalConstraint: MiningConstraint
  ): Task[Unit] = {
    generateOneMicroBlockTask(account, accumulatedBlock, constraints, restTotalConstraint)
      .asyncBoundary(minerScheduler)
      .timed
      .delayExecution(delay)
      .flatMap {
        case (t, Success(newBlock, newConstraint)) =>
          log.info(f"Next mining scheduled in ${(settings.microBlockInterval - t).toUnit(SECONDS)}%.3f seconds")
          generateMicroBlockSequence(account, newBlock, settings.microBlockInterval - t, constraints, newConstraint)
        case (_, Retry) =>
          generateMicroBlockSequence(account, accumulatedBlock, settings.microBlockInterval, constraints, restTotalConstraint)
        case (_, Stop) =>
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
  ): Task[MicroBlockMiningResult] =
    Task
      .defer {
        val (maybeUnconfirmed, updatedTotalConstraint) = Instrumented.logMeasure(log, "packing unconfirmed transactions for microblock") {
          val mdConstraint                       = MultiDimensionalMiningConstraint(restTotalConstraint, constraints.micro)
          val (unconfirmed, updatedMdConstraint) = utx.packUnconfirmed(mdConstraint, settings.maxPackTime)
          (unconfirmed, updatedMdConstraint.constraints.head)
        }

        maybeUnconfirmed match {
          case None =>
            log.trace("UTX is empty, retrying")
            Task.now(Retry)

          case Some(unconfirmed) if unconfirmed.nonEmpty =>
            log.trace(s"Generating microBlock for $account, constraints: $updatedTotalConstraint")

            for {
              blocks <- forgeBlocks(account, accumulatedBlock, unconfirmed)
                .leftWiden[Throwable]
                .liftTo[Task]
              (signedBlock, microBlock) = blocks
              blockId <- appendMicroBlock(microBlock)
              _ <- broadcastMicroBlock(account, microBlock, blockId)
            } yield {
              if (updatedTotalConstraint.isFull) Stop
              else Success(signedBlock, updatedTotalConstraint)
            }
          case _ =>
            if (updatedTotalConstraint.isFull) {
              log.trace(s"Stopping forging microBlocks, the block is full: $updatedTotalConstraint")
              Task.now(Stop)
            } else {
              log.trace("All transactions are too big")
              Task.now(Retry)
            }
        }
      }

  private def broadcastMicroBlock(account: KeyPair, microBlock: MicroBlock, blockId: BlockId): Task[Unit] =
    Task(allChannels.broadcast(MicroBlockInv(account, blockId, microBlock.reference)))

  private def appendMicroBlock(microBlock: MicroBlock): Task[BlockId] =
    appendMicroBlockTask(microBlock)
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
            version = accumulatedBlock.header.version,
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
