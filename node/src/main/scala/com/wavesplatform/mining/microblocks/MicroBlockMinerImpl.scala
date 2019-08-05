package com.wavesplatform.mining.microblocks

import java.util.concurrent.TimeUnit

import cats.data.EitherT
import cats.effect.{Clock, Sync}
import cats.effect.concurrent.Ref
import cats.implicits._
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.lang.ValidationError
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

  private def packTransactionsForMicroblock(constraints: MiningConstraints, constraint: MiningConstraint): Task[TransactionPackResult] = {
    for {
      start <- Task.clock(minerScheduler).realTime(TimeUnit.MILLISECONDS)
      tup <- measureLogF[Task, (Seq[Transaction], MiningConstraint)](log, "packing unconfirmed transactions for microblock") {
        val mdConstraint                       = MultiDimensionalMiningConstraint(constraint, constraints.micro)
        val (unconfirmed, updatedMdConstraint) = utx.packUnconfirmed(mdConstraint, settings.maxPackTime)
        (unconfirmed, updatedMdConstraint.constraints.head)
      }
      (unconfirmed, updatedTotalConstraint) = tup
      finish <- Task.clock.realTime(TimeUnit.MILLISECONDS)
    } yield TransactionPackResult(unconfirmed, updatedTotalConstraint, (finish - start).millis)
  }

  def generateMicroBlockSequence(
      account: KeyPair,
      accumulatedBlock: Block,
      delay: FiniteDuration,
      constraints: MiningConstraints,
      restTotalConstraint: MiningConstraint
  ): Task[Unit] = {

    val miningResult = for {
      _          <- debugState.set(MinerDebugInfo.MiningMicroblocks)
      packResult <- packTransactionsForMicroblock(constraints, restTotalConstraint)
      result <- generateOneMicroBlockTask(account, accumulatedBlock, packResult.transactions, restTotalConstraint)
        .asyncBoundary(minerScheduler)
        .delayExecution(delay - packResult.timeSpent)
    } yield result

    miningResult.flatMap {
      case Success(newTotal, updatedTotalConstraint) =>
        generateMicroBlockSequence(account, newTotal, settings.microBlockInterval, constraints, updatedTotalConstraint)
      case Delay(d) =>
        Task.delay {
          log.trace(
            s"Quorum not available (${allChannels.size()}/${settings.quorum}), not forging microblock with ${account.address}, next attempt in 5 seconds"
          )
        } >> generateMicroBlockSequence(account, accumulatedBlock, 0.seconds, constraints, restTotalConstraint).delayExecution(d)
      case Retry =>
        Task.delay {
          log.trace(s"Skipping microBlock because utx is empty")
        } >> generateMicroBlockSequence(account, accumulatedBlock, settings.microBlockInterval, constraints, restTotalConstraint)
      case Error(e) =>
        debugState
          .set(MinerDebugInfo.Error(e.toString)) >>
          Task.delay {
            log.warn("Error mining MicroBlock: " + e.toString)
          }
      case Stop =>
        debugState
          .set(MinerDebugInfo.MiningBlocks) >>
          Task.delay {
            log.trace {
              if (restTotalConstraint.isFull) s"Stopping forging microBlocks, the block is full: $restTotalConstraint"
              else "Stopping forging microBlocks, because all transactions are too big"
            }
            log.debug("MicroBlock mining completed, block is full")
          }
    }
  }

  private def generateOneMicroBlockTask(account: KeyPair,
                                        accumulatedBlock: Block,
                                        unconfirmed: Seq[Transaction],
                                        updatedConstraint: MiningConstraint): Task[MicroBlockMiningResult] = {
    Task
      .delay(allChannels.size())
      .flatMap { pc =>
        if (pc < settings.quorum) Task.now(Delay(settings.noQuorumMiningDelay))
        else if (unconfirmed.isEmpty) Task.now(Stop)
        else {
          log.trace(s"Generating microBlock for $account, constraints: $updatedConstraint")

          val clockET = Task
            .clock(minerScheduler)
            .mapK(EitherT.liftK[Task, ValidationError])

          (for {
            start  <- clockET.realTime(TimeUnit.MILLISECONDS)
            blocks <- forgeMicroBlock(account, accumulatedBlock, unconfirmed)
            finish <- clockET.realTime(TimeUnit.MILLISECONDS)
            _      <- EitherT.liftF(microBlockBuildTimeStats.safeRecordT(finish - start))
            (signedBlock, microBlock) = blocks
            _ <- appendMicroBlock(microBlock)
            _ <- broadcastMicroBlock(account, microBlock)
          } yield {
            if (updatedConstraint.isFull) Stop
            else Success(signedBlock, updatedConstraint)
          }).valueOr(Error)
        }
      }
  }

  private def broadcastMicroBlock(account: KeyPair, microBlock: MicroBlock): EitherT[Task, ValidationError, Unit] =
    EitherT.liftF[Task, ValidationError, Unit](Task.delay {
      allChannels.broadcast(MicroBlockInv(account, microBlock.totalResBlockSig, microBlock.prevResBlockSig))
    })

  private def appendMicroBlock(microBlock: MicroBlock): EitherT[Task, ValidationError, Unit] =
    EitherT(MicroblockAppender(blockchainUpdater, utx, appenderScheduler, verify = false)(microBlock))

  private def forgeMicroBlock(
      account: KeyPair,
      accumulatedBlock: Block,
      unconfirmed: Seq[Transaction]
  ): EitherT[Task, ValidationError, (Block, MicroBlock)] = EitherT {
    Task.delay {
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
      } yield (signedBlock, microBlock)
    }
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
  final case class Delay(d: FiniteDuration)                             extends MicroBlockMiningResult
  final case class Error(e: ValidationError)                            extends MicroBlockMiningResult
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
