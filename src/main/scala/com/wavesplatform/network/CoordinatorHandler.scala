package com.wavesplatform.network

import com.wavesplatform.Coordinator
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.group.ChannelGroup
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.block.Block
import scorex.transaction.ValidationError
import scorex.utils.ScorexLogging

@Sharable
class CoordinatorHandler(coordinator: Coordinator, peerDatabase: PeerDatabase, allChannels: ChannelGroup)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  import CoordinatorHandler._

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case c: Checkpoint =>
      loggingResult(id(ctx), "applying checkpoint", coordinator.processCheckpoint(c)).fold(
        _ => peerDatabase.blacklistAndClose(ctx.channel()),
        score => allChannels.broadcast(LocalScoreChanged(score), Some(ctx.channel()))
      )
    case ExtensionBlocks(blocks) =>
      loggingResult(id(ctx), "processing fork", coordinator.processFork(blocks.head.reference, blocks))
        .fold(
          _ => peerDatabase.blacklistAndClose(ctx.channel()),
          score => allChannels.broadcast(LocalScoreChanged(score))
        )
    case b: Block =>
      if (b.signatureValid) {
        loggingResult(id(ctx), "applying block", coordinator.processBlock(b, local = false))
          .foreach(score => allChannels.broadcast(LocalScoreChanged(score)))
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
