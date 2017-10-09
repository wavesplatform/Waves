package com.wavesplatform.network

import java.util.concurrent.Executors
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.metrics.{BlockStats, HistogramExt}
import com.wavesplatform.mining.Miner
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.{Coordinator, UtxPool}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.group.ChannelGroup
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import kamon.Kamon
import scorex.block.{Block, MicroBlock}
import scorex.transaction.ValidationError.InvalidSignature
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

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
                         featureProvider: FeatureProvider)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private val counter = new AtomicInteger
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor { r =>
    val t = new Thread(r)
    t.setName(s"coordinator-handler-${counter.incrementAndGet()}")
    t.setDaemon(true)
    t
  })

  private val processCheckpoint = Coordinator.processCheckpoint(checkpointService, history, blockchainUpdater) _
  private val processFork = Coordinator.processFork(checkpointService, history, blockchainUpdater, stateReader, utxStorage, time, settings, blockchainReadiness, featureProvider) _
  private val processBlock = Coordinator.processSingleBlock(checkpointService, history, blockchainUpdater, time, stateReader, utxStorage, blockchainReadiness, settings, featureProvider) _
  private val processMicroBlock = Coordinator.processMicroBlock(checkpointService, history, blockchainUpdater, utxStorage) _


  private def scheduleMiningAndBroadcastScore(score: BigInt): Unit = {
    miner.scheduleMining()
    allChannels.broadcast(LocalScoreChanged(score))
  }

  private def processAndBlacklistOnFailure[A](src: Channel, start: => String, success: => String, errorPrefix: String,
                                              f: => Either[_, Option[A]],
                                              r: A => Unit): Unit = {
    log.debug(start)
    Future(f) onComplete {
      case Success(Right(maybeNewScore)) =>
        log.debug(success)
        maybeNewScore.foreach(r)
      case Success(Left(ve)) =>
        log.warn(s"$errorPrefix: $ve")
        peerDatabase.blacklistAndClose(src, s"$errorPrefix: $ve")
      case Failure(t) => rethrow(errorPrefix, t)
    }
  }

  private def rethrow(msg: String, failure: Throwable) = throw new Exception(msg, failure.getCause)

  private def warnAndBlacklist(msg: => String, ch: Channel): Unit = {
    log.warn(msg)
    peerDatabase.blacklistAndClose(ch, msg)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case c: Checkpoint => processAndBlacklistOnFailure(ctx.channel,
      "Attempting to process checkpoint",
      "Successfully processed checkpoint",
      s"Error processing checkpoint",
      processCheckpoint(c).map(Some(_)), scheduleMiningAndBroadcastScore)

    case ExtensionBlocks(blocks) => processAndBlacklistOnFailure(ctx.channel,
      s"Attempting to append extension ${formatBlocks(blocks)}",
      s"Successfully appended extension ${formatBlocks(blocks)}",
      s"Error appending extension ${formatBlocks(blocks)}",
      processFork(blocks), scheduleMiningAndBroadcastScore)

    case b: Block => Future({
      BlockStats.received(b, BlockStats.Source.Broadcast, ctx)
      CoordinatorHandler.blockReceivingLag.safeRecord(System.currentTimeMillis() - b.timestamp)
      Signed.validateSignatures(b).flatMap(b => processBlock(b, false))
    }) onComplete {
      case Success(Right(None)) =>
        log.trace(s"Block ${b.uniqueId} already appended")
      case Success(Right(Some(newScore))) =>
          log.debug(s"Appended block ${b.uniqueId}")
          if (b.transactionData.isEmpty)
            allChannels.broadcast(BlockForged(b), Some(ctx.channel()))
          miner.scheduleMining()
          allChannels.broadcast(LocalScoreChanged(newScore))
      case Success(Left(is: InvalidSignature)) =>
        warnAndBlacklist(s"Could not append block ${b.uniqueId}: $is", ctx.channel())
      case Success(Left(ve)) =>
        BlockStats.declined(b, BlockStats.Source.Broadcast)
        log.debug(s"Could not append block ${b.uniqueId}: $ve")
      // no need to push anything downstream in here, because channels are pinned only when handling extensions
      case Failure(t) => rethrow(s"Error appending block ${b.uniqueId}", t)
    }

    case MicroBlockResponse(m) => Future({
      Signed.validateSignatures(m).flatMap(m => processMicroBlock(m))
    }) onComplete {
      case Success(Right(())) =>
        allChannels.broadcast(MicroBlockInv(m.totalResBlockSig, m.prevResBlockSig), Some(ctx.channel()))
        BlockStats.applied(m)
      case Success(Left(is: InvalidSignature)) =>
        warnAndBlacklist(s"Could not append microblock ${m.totalResBlockSig}: $is", ctx.channel())
      case Success(Left(ve)) =>
        BlockStats.declined(m)
        log.debug(s"Could not append microblock ${m.totalResBlockSig}: $ve")
      case Failure(t) => rethrow(s"Error appending microblock ${m.totalResBlockSig}", t)
    }
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
