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
  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = {
    log.debug(s"${id(ctx)} removed: ${ctx.isRemoved}, active: ${ctx.channel().isActive}, open: ${ctx.channel().isOpen}")
    msg match {
      case c: Checkpoint =>
        val result = handleResult(ctx, "applying checkpoint", coordinator.processCheckpoint(c))
        result.left.foreach(_ => peerDatabase.blacklistAndClose(ctx.channel()))
        result.foreach(_ => allChannels.broadcast(c, Some(ctx.channel())))
      case ExtensionBlocks(blocks) =>
        handleResult(ctx, "processing fork", coordinator.processFork(blocks.head.reference, blocks))
          .left.foreach(_ => peerDatabase.blacklistAndClose(ctx.channel()))
      case b: Block =>
        handleResult(ctx, "applying block", coordinator.processBlock(b))
          .left.foreach(_ => peerDatabase.blacklistAndClose(ctx.channel()))
      case bf@BlockForged(b) =>
        handleResult(ctx, "applying locally mined block", coordinator.processBlock(b))
          .foreach(_ => allChannels.broadcast(bf))
      case other =>
        log.debug(other.getClass.getCanonicalName)
    }
  }
}

object CoordinatorHandler extends ScorexLogging {
  private[CoordinatorHandler] def handleResult(
      ctx: ChannelHandlerContext,
      msg: String,
      f: => Either[ValidationError, BigInt]): Either[ValidationError, BigInt] = {
    log.debug(s"${id(ctx)} Starting $msg")
    val result = f
    result match {
      case Left(error) =>
        log.warn(s"${id(ctx)} Error $msg: $error")
      case Right(newScore) =>
        log.debug(s"${id(ctx)} Finished $msg, new local score is $newScore")
        ctx.writeAndFlush(LocalScoreChanged(newScore))
    }
    result
  }
}
