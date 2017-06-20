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
      loggingResult(ctx, "applying checkpoint", coordinator.processCheckpoint(c)).fold(
        _ => peerDatabase.blacklistAndClose(ctx.channel()),
        score => allChannels.broadcast(LocalScoreChanged(score), Some(ctx.channel()))
      )
    case ExtensionBlocks(blocks) =>
      loggingResult(ctx, "processing fork", coordinator.processFork(blocks.head.reference, blocks))
        .fold(
          _ => peerDatabase.blacklistAndClose(ctx.channel()),
          score => allChannels.broadcast(LocalScoreChanged(score))
        )
    case b: Block =>
      loggingResult(ctx, "applying block", coordinator.processBlock(b)).fold(
        _ => peerDatabase.blacklistAndClose(ctx.channel()),
        score => allChannels.broadcast(LocalScoreChanged(score))
      )
    // "off-chain" messages: locally forged block and checkpoints from API
    case bf@BlockForged(b) =>
      loggingResult(ctx, "applying locally mined block", coordinator.processBlock(b))
        .foreach(_ => allChannels.broadcast(bf))
    case OffChainCheckpoint(c, p) =>
      loggingResult(ctx, "processing checkpoint from API", coordinator.processCheckpoint(c)).fold(
        e => p.success(Left(e)),
        { score =>
          p.success(Right(c))
          allChannels.broadcast(LocalScoreChanged(score))
        }
      )
  }
}

object CoordinatorHandler extends ScorexLogging {
  private[CoordinatorHandler] def loggingResult(ctx: ChannelHandlerContext, msg: String, f: => Either[ValidationError, BigInt]) = {
    log.debug(s"${id(ctx)} Starting $msg")
    val result = f
    result match {
      case Left(error) => log.warn(s"${id(ctx)} Error $msg: $error")
      case Right(newScore) => log.debug(s"${id(ctx)} Finished $msg, new local score is $newScore")
    }
    result
  }
}
