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
import monix.execution.Scheduler
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
                         microBlockOwners: MicroBlockOwners,
                         invalidBlocks: InvalidBlockStorage,
                         scheduler: Scheduler)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private val processCheckpoint = Coordinator.processCheckpoint(checkpointService, history, blockchainUpdater) _
  private val processFork = Coordinator.processFork(checkpointService, history, blockchainUpdater, stateReader, utxStorage, time, settings, blockchainReadiness, featureProvider, invalidBlocks) _
  private val processBlock = Coordinator.processSingleBlock(checkpointService, history, blockchainUpdater, time, stateReader, utxStorage, settings.blockchainSettings, featureProvider) _
  private val processMicroBlock = Coordinator.processMicroBlock(checkpointService, history, blockchainUpdater, utxStorage) _

  private def scheduleMiningAndBroadcastScore(score: BigInt, breakExtProcessing: Boolean): Unit = {
    miner.scheduleMining()
    allChannels.broadcast(LocalScoreChanged(score, breakExtProcessing))
  }

  private def processAndBlacklistOnFailure(ctx: ChannelHandlerContext,
                                           start: => String,
                                           success: => String,
                                           errorPrefix: String,
                                           f: => Either[_, BigInt]): Unit = {
    log.debug(s"${id(ctx)} $start")
    Task(f).map {
      case Right(newScore) =>
        log.debug(s"${id(ctx)} $success")
        scheduleMiningAndBroadcastScore(newScore, breakExtProcessing = true)
      case Left(ve) =>
        log.warn(s"${id(ctx)} $errorPrefix: $ve")
        peerDatabase.blacklistAndClose(ctx.channel(), s"$errorPrefix: $ve")
    }.onErrorHandle[Unit](ctx.fireExceptionCaught).runAsync(scheduler)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case c: Checkpoint => processAndBlacklistOnFailure(ctx,
      "Attempting to process checkpoint",
      "Successfully processed checkpoint",
      "Error processing checkpoint",
      processCheckpoint(c))

    case ExtensionBlocks(blocks) =>
      blocks.foreach(BlockStats.received(_, BlockStats.Source.Ext, ctx))
      processAndBlacklistOnFailure(ctx,
        s"Attempting to append extension ${formatBlocks(blocks)}",
        s"Successfully appended extension ${formatBlocks(blocks)}",
        s"Error appending extension ${formatBlocks(blocks)}",
        processFork(blocks))

    case b: Block => (Task {
      BlockStats.received(b, BlockStats.Source.Broadcast, ctx)
      CoordinatorHandler.blockReceivingLag.safeRecord(System.currentTimeMillis() - b.timestamp)
      b.signaturesValid().flatMap(b => processBlock(b))
    } map {
      case Right(None) =>
        log.trace(s"${id(ctx)} $b already appended")
      case Right(Some(newScore)) =>
        BlockStats.applied(b, BlockStats.Source.Broadcast, history.height())
        Coordinator.updateBlockchainReadinessFlag(history, time, blockchainReadiness, settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed)
        log.debug(s"${id(ctx)} Appended $b")
        if (b.transactionData.isEmpty)
          allChannels.broadcast(BlockForged(b), Some(ctx.channel()))
        scheduleMiningAndBroadcastScore(newScore, breakExtProcessing = false)
      case Left(is: InvalidSignature) =>
        peerDatabase.blacklistAndClose(ctx.channel(), s"Could not append $b: $is")
      case Left(ve) =>
        BlockStats.declined(b, BlockStats.Source.Broadcast)
        log.debug(s"${id(ctx)} Could not append $b: $ve")
    }).onErrorHandle[Unit](ctx.fireExceptionCaught).runAsync(scheduler)

    case md: MicroblockData =>
      import md.microBlock
      val microblockTotalResBlockSig = microBlock.totalResBlockSig
      (Task(microBlock.signaturesValid().flatMap(processMicroBlock)) map {
        case Right(()) =>
          md.invOpt match {
            case Some(mi) => allChannels.broadcast(mi, microBlockOwners.all(microBlock.totalResBlockSig).map(_.channel()))
            case None => log.warn(s"${id(ctx)} Not broadcasting MicroBlockInv")
          }
          BlockStats.applied(microBlock)
        case Left(is: InvalidSignature) =>
          peerDatabase.blacklistAndClose(ctx.channel(), s"Could not append microblock $microblockTotalResBlockSig: $is")
        case Left(ve) =>
          BlockStats.declined(microBlock)
          log.debug(s"${id(ctx)} Could not append microblock $microblockTotalResBlockSig: $ve")
      }).onErrorHandle[Unit](ctx.fireExceptionCaught).runAsync(scheduler)
  }
}

object CoordinatorHandler extends ScorexLogging {
  private val blockReceivingLag = Kamon.metrics.histogram("block-receiving-lag")
}
