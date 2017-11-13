package com.wavesplatform.network

import com.wavesplatform.state2.ByteStr
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import scorex.block.Block
import scorex.utils.ScorexLogging

class OptimisticExtensionLoader extends ChannelDuplexHandler with ScorexLogging {

  private var hopefullyNextIds = Seq.empty[ByteStr]
  private var nextExtensionBlocks = Seq.empty[Block]
  private var requestedLocalIds = Seq.empty[ByteStr]

  private def loadNextPart(ctx: ChannelHandlerContext, blocks: Seq[Block]): Unit = if (blocks.size > 1) {
    requestedLocalIds = Seq.empty
    hopefullyNextIds = blocks.view.map(_.uniqueId).reverseIterator.take(100).toSeq
    log.debug(s"${id(ctx)} Loading next part, sending ${hopefullyNextIds.size} signatures")
    ctx.writeAndFlush(LoadBlockchainExtension(hopefullyNextIds))
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case ExtensionBlocks(extension) if extension.isEmpty =>
      requestedLocalIds = Seq.empty
      log.debug(s"${id(ctx)} Blockchain is up to date")
      super.channelRead(ctx, msg)
    case ExtensionBlocks(extension) if requestedLocalIds.isEmpty =>
      nextExtensionBlocks = extension
      log.debug(s"${id(ctx)} Loaded new extension with ${extension.size} blocks, keeping for now")
    case ExtensionBlocks(extension) if requestedLocalIds.contains(extension.head.reference) =>
      loadNextPart(ctx, extension)
      log.debug(s"${id(ctx)} Passing extension with ${extension.size} signatures upstream")
      super.channelRead(ctx, msg)
    case ExtensionBlocks(extension) =>
      log.debug(s"${id(ctx)} Discarding ${extension.size} blocks")
    case _ => super.channelRead(ctx, msg)
  }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = msg match {
    case LoadBlockchainExtension(localIds) =>
      requestedLocalIds = localIds

      if (nextExtensionBlocks.nonEmpty && localIds.contains(nextExtensionBlocks.head.reference)) {
        loadNextPart(ctx, nextExtensionBlocks)
        ctx.fireChannelRead(ExtensionBlocks(nextExtensionBlocks))
      } else if (hopefullyNextIds.exists(localIds.contains)) {
        log.debug(s"${id(ctx)} Still waiting for extension to load")
      } else {
        super.write(ctx, msg, promise)
      }

      nextExtensionBlocks = Seq.empty

    case _ => super.write(ctx, msg, promise)
  }
}
