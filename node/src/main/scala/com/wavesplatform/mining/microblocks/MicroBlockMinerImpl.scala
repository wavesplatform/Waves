package com.wavesplatform.mining.microblocks

import java.util.concurrent.TimeUnit

import cats.data.EitherT
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Sync}
import cats.implicits._
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.metrics._
import com.wavesplatform.mining.microblocks.MicroBlockMinerImpl._
import com.wavesplatform.mining.{MinerDebugInfo, MiningConstraint, MiningConstraints, MultiDimensionalMiningConstraint}
import com.wavesplatform.network.{MicroBlockInv, _}
import com.wavesplatform.settings.MinerSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.appender.MicroblockAppender
import com.wavesplatform.transaction.{BlockchainUpdater, Transaction}
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging}
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import kamon.metric.TimerMetric
import monix.eval.Task
import monix.execution.schedulers.SchedulerService

import scala.concurrent.duration._

class MicroBlockMinerImpl(debugState: Ref[Task, MinerDebugInfo.State],
                          allChannels: ChannelGroup,
                          blockchainUpdater: BlockchainUpdater with Blockchain,
                          utx: UtxPool,
                          settings: MinerSettings,
                          minerScheduler: SchedulerService,
                          appenderScheduler: SchedulerService)
    extends MicroBlockMiner
    with ScorexLogging {

  val microBlockBuildTimeStats: TimerMetric = Kamon.timer("miner.forge-microblock-time")

  def generateMicroBlockSequence(
      account: KeyPair,
      accumulatedBlock: Block,
      delay: FiniteDuration,
      constraints: MiningConstraints,
      restTotalConstraint: MiningConstraint
  ): Task[Unit] = {
    generateOneMicroBlockTask(account, accumulatedBlock, constraints, restTotalConstraint).timed
      .delayExecution(delay)
      .flatMap {
        case (t, Success(newTotal, updatedTotalConstraint)) =>
          generateMicroBlockSequence(account, newTotal, settings.microBlockInterval - t, constraints, updatedTotalConstraint)
        case (t, Retry) =>
          generateMicroBlockSequence(account, accumulatedBlock, settings.microBlockInterval - t, constraints, restTotalConstraint)
        case (_, Stop) =>
          debugState
            .set(MinerDebugInfo.MiningBlocks) >>
            Task.delay {
              log.debug("MicroBlock mining completed, block is full")
            }
      }
  }

  private def generateOneMicroBlockTask(account: KeyPair,
                                        accumulatedBlock: Block,
                                        constraints: MiningConstraints,
                                        restTotalConstraint: MiningConstraint): Task[MicroBlockMiningResult] = {
    Task
      .defer {
        val (unconfirmed, updatedTotalConstraint) = Instrumented.logMeasure(log, "packing unconfirmed transactions for microblock") {
          val mdConstraint                       = MultiDimensionalMiningConstraint(restTotalConstraint, constraints.micro)
          val (unconfirmed, updatedMdConstraint) = utx.packUnconfirmed(mdConstraint, settings.maxPackTime)
          (unconfirmed, updatedMdConstraint.constraints.head)
        }

        if (unconfirmed.isEmpty && updatedTotalConstraint.isFull) {
          log.trace {
            if (updatedTotalConstraint.isFull) s"Stopping forging microBlocks, the block is full: $updatedTotalConstraint"
            else "Stopping forging microBlocks, because all transactions are too big"
          }
          Task.now(Stop)
        } else if (unconfirmed.isEmpty) {
          log.trace(s"Skipping microBlock because utx is empty")
          Task.now(Retry)
        } else {
          log.trace(s"Generating microBlock for $account, constraints: $updatedTotalConstraint")

          val miningTask = for {
            blocks <- forgeBlocks(account, accumulatedBlock, unconfirmed)
              .leftWiden[Throwable]
              .raiseOrPure[Task]
            (signedBlock, microBlock) = blocks
            _ <- appendMicroBlock(microBlock)
            _ <- broadcastMicroBlock(account, microBlock)
          } yield signedBlock

          miningTask.map { accumulatedBlock =>
            if (updatedTotalConstraint.isFull) Stop
            else Success(accumulatedBlock, updatedTotalConstraint)
          }
        }
      }
  }

  private def broadcastMicroBlock(account: KeyPair, microBlock: MicroBlock): Task[Unit] =
    Task.delay {
      allChannels.broadcast(MicroBlockInv(account, microBlock.totalResBlockSig, microBlock.prevResBlockSig))
    }

  private def appendMicroBlock(microBlock: MicroBlock): Task[Unit] =
    MicroblockAppender(blockchainUpdater, utx, appenderScheduler, verify = false)(microBlock)
      .flatMap {
        case Left(err) => Task.raiseError(MicroBlockAppendError(microBlock, err))
        case Right(v)  => Task.now(v)
      }

  private def forgeBlocks(
      account: KeyPair,
      accumulatedBlock: Block,
      unconfirmed: Seq[Transaction]
  ): MicroBlockMiningError Either (Block, MicroBlock) =
    microBlockBuildTimeStats.measureSuccessful {
      for {
        signedBlock <- Block
          .buildAndSign(
            version = 3,
            timestamp = accumulatedBlock.timestamp,
            reference = accumulatedBlock.reference,
            consensusData = accumulatedBlock.consensusData,
            transactionData = accumulatedBlock.transactionData ++ unconfirmed,
            signer = account,
            featureVotes = accumulatedBlock.featureVotes
          )
          .leftMap(BlockBuildError)
        microBlock <- MicroBlock
          .buildAndSign(
            account,
            unconfirmed,
            accumulatedBlock.signerData.signature,
            signedBlock.signerData.signature
          )
          .leftMap(MicroBlockBuildError)
      } yield (signedBlock, microBlock)
    }
}

object MicroBlockMinerImpl {
  case class TransactionPackResult(
      transactions: Seq[Transaction],
      updatedConstraint: MiningConstraint,
      timeSpent: FiniteDuration
  )

  sealed trait MicroBlockMiningResult

  case object Stop                                                      extends MicroBlockMiningResult
  case object Retry                                                     extends MicroBlockMiningResult
  final case class Success(b: Block, totalConstraint: MiningConstraint) extends MicroBlockMiningResult

  def measureLogF[F[_]: Sync: Clock, A](log: LoggerFacade, action: String)(fa: => A): F[A] =
    for {
      start  <- Clock[F].realTime(TimeUnit.MILLISECONDS)
      result <- Sync[F].delay(fa)
      finish <- Clock[F].realTime(TimeUnit.MILLISECONDS)
      _ <- Sync[F].delay {
        log.trace(s"$action took ${finish - start}ms")
      }
    } yield result
}
