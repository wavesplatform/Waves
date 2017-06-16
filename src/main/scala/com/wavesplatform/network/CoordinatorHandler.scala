package com.wavesplatform.network

import com.wavesplatform.Coordinator
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.block.Block
import scorex.utils.ScorexLogging

@Sharable
class CoordinatorHandler(coordinator: Coordinator) extends ChannelInboundHandlerAdapter with ScorexLogging {
  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = {
    log.debug(s"${id(ctx)} removed: ${ctx.isRemoved}, active: ${ctx.channel().isActive}, open: ${ctx.channel().isOpen}")
    msg match {
      case c: Checkpoint =>
        coordinator.processCheckpoint(c)
        // todo: check result
      case ExtensionBlocks(blocks) =>
        log.debug(s"${id(ctx)} Processing fork")
        coordinator.processFork(blocks.head.reference, blocks)
        // todo: check result, send score downstream
      case b: Block =>
        coordinator.processBlock(b)
        // todo: check result, send score downstream
      case other => log.debug(other.getClass.getCanonicalName)
    }
  }
}
