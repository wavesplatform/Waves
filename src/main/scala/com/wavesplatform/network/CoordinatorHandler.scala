package com.wavesplatform.network

import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.metrics.{BlockStats, HistogramExt}
import com.wavesplatform.mining.Miner
import com.wavesplatform.network.MicroBlockSynchronizer.MicroblockData
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.StateReader
import com.wavesplatform.{Coordinator, UtxPool}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.group.ChannelGroup
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import kamon.Kamon
import monix.eval.Task
import scorex.block.Block
import scorex.transaction.ValidationError.InvalidSignature
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

@Sharable
class CoordinatorHandler(checkpointService: CheckpointService,
                         history: History,
                         blockchainUpdater: BlockchainUpdater,
                         time: Time,
                         stateReader: StateReader,
                         utxStorage: UtxPool,
                         blockchainReadiness:
                         AtomicBoolean,
                         miner: Miner,
                         settings: WavesSettings,
                         peerDatabase: PeerDatabase,
                         allChannels: ChannelGroup,
                         featureProvider: FeatureProvider,
                         microBlockOwners: MicroBlockOwners)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private implicit val scheduler = monix.execution.Scheduler.singleThread("coordinator-handler", reporter = com.wavesplatform.utils.UncaughtExceptionsToLogReporter)

  private val processCheckpoint = Coordinator.processCheckpoint(checkpointService, history, blockchainUpdater) _
  private val processFork = Coordinator.processFork(checkpointService, history, blockchainUpdater, stateReader, utxStorage, time, settings, blockchainReadiness, featureProvider) _
  private val processBlock = Coordinator.processSingleBlock(checkpointService, history, blockchainUpdater, time, stateReader, utxStorage, settings.blockchainSettings, featureProvider) _
  private val processMicroBlock = Coordinator.processMicroBlock(checkpointService, history, blockchainUpdater, utxStorage) _

  private def scheduleMiningAndBroadcastScore(score: BigInt): Unit = {
    miner.scheduleMining()
    allChannels.broadcast(LocalScoreChanged(score))
  }

  private def processAndBlacklistOnFailure[A](ctx: ChannelHandlerContext, start: => String, success: => String, errorPrefix: String,
                                              f: => Either[_, Option[A]],
                                              r: A => Unit): Unit = {
    log.debug(start)
    Task(f).map {
      case Right(maybeNewScore) =>
        log.debug(success)
        maybeNewScore.foreach(r)
      case Left(ve) =>
        log.warn(s"$errorPrefix: $ve")
        peerDatabase.blacklistAndClose(ctx.channel(), s"$errorPrefix: $ve")
    }.onErrorHandle[Unit](ctx.fireExceptionCaught).runAsync
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case c: Checkpoint => processAndBlacklistOnFailure(ctx,
      "Attempting to process checkpoint",
      "Successfully processed checkpoint",
      s"Error processing checkpoint",
      processCheckpoint(c).map(Some(_)), scheduleMiningAndBroadcastScore)

    case ExtensionBlocks(blocks) =>
      blocks.foreach(BlockStats.received(_, BlockStats.Source.Ext, ctx))
      processAndBlacklistOnFailure(ctx,
        s"Attempting to append extension ${formatBlocks(blocks)}",
        s"Successfully appended extension ${formatBlocks(blocks)}",
        s"Error appending extension ${formatBlocks(blocks)}",
        processFork(blocks),
        scheduleMiningAndBroadcastScore)

    case b: Block => (Task {
      BlockStats.received(b, BlockStats.Source.Broadcast, ctx)
      CoordinatorHandler.blockReceivingLag.safeRecord(System.currentTimeMillis() - b.timestamp)
      b.signaturesValid().flatMap(b => processBlock(b))
    } map {
      case Right(None) =>
        log.trace(s"$b already appended")
      case Right(Some(newScore)) =>
        BlockStats.applied(b, BlockStats.Source.Broadcast, history.height())
        Coordinator.updateBlockchainReadinessFlag(history, time, blockchainReadiness, settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed)
        log.debug(s"Appended $b")
        if (b.transactionData.isEmpty)
          allChannels.broadcast(BlockForged(b), Some(ctx.channel()))
        scheduleMiningAndBroadcastScore(newScore)
      case Left(is: InvalidSignature) =>
        peerDatabase.blacklistAndClose(ctx.channel(), s"Could not append $b: $is")
      case Left(ve) =>
        BlockStats.declined(b, BlockStats.Source.Broadcast)
        log.debug(s"Could not append $b: $ve")
    }).onErrorHandle[Unit](ctx.fireExceptionCaught).runAsync

    case md: MicroblockData =>
      import md.microBlock
      val microblockTotalResBlockSig = microBlock.totalResBlockSig
      (Task(microBlock.signaturesValid().flatMap(processMicroBlock)) map {
        case Right(()) =>
          md.invOpt match {
            case Some(mi) => allChannels.broadcast(mi, microBlockOwners.all(microBlock.totalResBlockSig).map(_.channel()))
            case None => log.warn("Not broadcasting MicroBlockInv")
          }
          BlockStats.applied(microBlock)
        case Left(is: InvalidSignature) =>
          peerDatabase.blacklistAndClose(ctx.channel(), s"Could not append microblock $microblockTotalResBlockSig: $is")
        case Left(ve) =>
          BlockStats.declined(microBlock)
          log.debug(s"Could not append microblock $microblockTotalResBlockSig: $ve")
      }).onErrorHandle[Unit](ctx.fireExceptionCaught).runAsync
  }
}

object CoordinatorHandler extends ScorexLogging {
  private val blockReceivingLag = Kamon.metrics.histogram("block-receiving-lag")

  def loggingResult[R](idCtx: String, msg: String, f: => Either[ValidationError, R]): Either[ValidationError, R] = {
    log.debug(s"$idCtx Starting $msg processing")
    val result = f
    result match {
      case Left(error) => log.warn(s"$idCtx Error processing $msg: $error")
      case Right(newScore) => log.debug(s"$idCtx Finished $msg processing, new local score is $newScore")
    }
    result
  }
}
