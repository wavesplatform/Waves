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
import scorex.transaction.{BlockchainUpdater, CheckpointService, History, ValidationError}
import scorex.utils.{ScorexLogging, Time}

@Sharable
class CoordinatorHandler(checkpointService: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, time: Time,
                         stateReader: StateReader, utxStorage: UtxPool, blockchainReadiness: AtomicBoolean, miner: Miner,
                         settings: WavesSettings, peerDatabase: PeerDatabase, allChannels: ChannelGroup)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  import CoordinatorHandler._

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case c: Checkpoint =>
      loggingResult(id(ctx), "applying checkpoint",
        Coordinator.processCheckpoint(checkpointService, history, blockchainUpdater, settings.checkpointsSettings.publicKey)(c))
        .fold(_ => peerDatabase.blacklistAndClose(ctx.channel()),
          score => allChannels.broadcast(ScoreChanged(score), Some(ctx.channel()))
        )
    case ExtensionBlocks(blocks) =>
      loggingResult(id(ctx), "processing fork",
        Coordinator.processFork(checkpointService, history, blockchainUpdater, stateReader, utxStorage, time, settings, miner, blockchainReadiness)(blocks))
        .fold(
          _ => peerDatabase.blacklistAndClose(ctx.channel()),
          score => allChannels.broadcast(ScoreChanged(score))
        )
    case b: Block =>
      if (b.signatureValid) {
        loggingResult(id(ctx), "applying block", Coordinator.processBlock(checkpointService, history, blockchainUpdater, time,
          stateReader, utxStorage, blockchainReadiness, miner, settings)(b, local = false))
          .foreach(score => allChannels.broadcast(ScoreChanged(score)))
      } else {
        peerDatabase.blacklistAndClose(ctx.channel())
      }
  }
}

object CoordinatorHandler extends ScorexLogging {
  def loggingResult(idCtx: String, msg: String, f: => Either[ValidationError, BigInt]): Either[ValidationError, BigInt] = {
    log.debug(s"$idCtx Starting $msg")
    val result = f
    result match {
      case Left(error) => log.warn(s"$idCtx Error $msg: $error")
      case Right(newScore) => log.debug(s"$idCtx Finished $msg, new local score is $newScore")
    }
    result
  }
}
