package com.wavesplatform.network

import com.wavesplatform.utils.ByteStr
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import scorex.block.Block
import scorex.utils.ScorexLogging

class OptimisticExtensionLoader extends ChannelDuplexHandler with ScorexLogging {

  private var hopefullyNextIds = Seq.empty[ByteStr]
  private var nextExtensionBlocks = Seq.empty[Block]
  private var blocksAreBeingLoaded = false
  private var discardNextBlocks = false

  private def loadNextPart(ctx: ChannelHandlerContext, blocks: Seq[Block]): Unit = {
    hopefullyNextIds = blocks.view.map(b => ByteStr(b.uniqueId)).reverseIterator.take(100).toSeq
    ctx.writeAndFlush(LoadBlockchainExtension(hopefullyNextIds))
    blocksAreBeingLoaded = true
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case ExtensionBlocks(extension) if discardNextBlocks =>
      blocksAreBeingLoaded = false
      discardNextBlocks = false
      log.debug(s"${id(ctx)} discarding just-loaded ${extension.length} blocks as requested")
    case ExtensionBlocks(extension) if hopefullyNextIds.isEmpty =>
      loadNextPart(ctx, extension)
      super.channelRead(ctx, msg)
    case ExtensionBlocks(extension) if extension.isEmpty =>
      hopefullyNextIds = Seq.empty
      nextExtensionBlocks = Seq.empty
      blocksAreBeingLoaded = false
    case ExtensionBlocks(extension) =>
      nextExtensionBlocks = extension
      blocksAreBeingLoaded = false
    case _ => super.channelRead(ctx, msg)
  }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) = msg match {
    case LoadBlockchainExtension(localIds) if hopefullyNextIds == localIds =>
      if (blocksAreBeingLoaded) {
        log.debug(s"${id(ctx)} Still waiting for extension to load")
        hopefullyNextIds = Seq.empty
      } else {
        log.debug(s"${id(ctx)} Extension already loaded")
        ctx.fireChannelRead(ExtensionBlocks(nextExtensionBlocks))
        loadNextPart(ctx, nextExtensionBlocks)
        nextExtensionBlocks = Seq.empty
      }
    case LoadBlockchainExtension(_) =>
      if (blocksAreBeingLoaded) {
        discardNextBlocks = true
        log.warn(s"${id(ctx)} Got unexpected known block ids, will discard extension once ready")
      }
      hopefullyNextIds = Seq.empty
      nextExtensionBlocks = Seq.empty
      super.write(ctx, msg, promise)
    case _ => super.write(ctx, msg, promise)
  }
}
