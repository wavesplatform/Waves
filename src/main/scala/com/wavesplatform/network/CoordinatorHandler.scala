package com.wavesplatform.network

import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.mining.Miner
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.{Coordinator, UtxPool}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.group.ChannelGroup
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.block.Block
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

@Sharable
class CoordinatorHandler(checkpointService: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, time: Time,
                         stateReader: StateReader, utxStorage: UtxPool, blockchainReadiness: AtomicBoolean, miner: Miner,
                         settings: WavesSettings, peerDatabase: PeerDatabase, allChannels: ChannelGroup)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  import CoordinatorHandler._

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case c: Checkpoint =>
      loggingResult(id(ctx), "Checkpoint",
        Coordinator.processCheckpoint(checkpointService, history, blockchainUpdater)(c))
        .fold(err => peerDatabase.blacklistAndClose(ctx.channel(), "Unable to process checkpoint due to " + err),
          score => allChannels.broadcast(LocalScoreChanged(score), Some(ctx.channel()))
        )
    case ExtensionBlocks(blocks) =>
      loggingResult(id(ctx), "ExtensionBlocks",
        Coordinator.processFork(checkpointService, history, blockchainUpdater, stateReader, utxStorage, time, settings, miner, blockchainReadiness)(blocks))
        .fold(
          err => peerDatabase.blacklistAndClose(ctx.channel(), "Unable to process ExtensionBlocks due to " + err),
          score => allChannels.broadcast(LocalScoreChanged(score))
        )
    case b: Block =>
      Signed.validateSignatures(b) match {
        case Right(_) =>
          loggingResult(id(ctx), "Block", Coordinator.processBlock(checkpointService, history, blockchainUpdater, time,
            stateReader, utxStorage, blockchainReadiness, miner, settings)(b, local = false))
            .foreach(score => allChannels.broadcast(LocalScoreChanged(score)))
        case Left(err) =>
          peerDatabase.blacklistAndClose(ctx.channel(), err.toString)
      }
    case MicroBlockResponse(m) =>
      Signed.validateSignatures(m) match {
        case Right(_) =>
          loggingResult(id(ctx), "MicroBlockResponse", Coordinator.processMicroBlock(checkpointService, history, blockchainUpdater, utxStorage)(m))
            .foreach(score => allChannels.broadcast(MicroBlockInv(m.totalResBlockSig), Some(ctx.channel())))
        case Left(err) =>
          peerDatabase.blacklistAndClose(ctx.channel(), err.toString)
      }
  }
}

object CoordinatorHandler extends ScorexLogging {
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
