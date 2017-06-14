package com.wavesplatform.network

import com.wavesplatform.state2.ByteStr
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import scorex.block.Block
import scorex.utils.ScorexLogging

class OptimisticExtensionLoader extends ChannelDuplexHandler with ScorexLogging {

  private var hopefullyNextIds = Seq.empty[ByteStr]
  private var nextExtensionBlocks = Seq.empty[Block]
  private var discardNextBlocks = false

  private def loadNextPart(ctx: ChannelHandlerContext, blocks: Seq[Block]): Unit = if (blocks.size > 1) {
    // Receiving just one block usually means we've reached the end of blockchain. Pre-Netty nodes
    // didn't handle GetSignatures(lastBlockId) message properly, hence the check.
    log.debug(s"${id(ctx)} loading next part")
    hopefullyNextIds = blocks.view.map(_.uniqueId).reverseIterator.take(100).toSeq
    ctx.writeAndFlush(LoadBlockchainExtension(hopefullyNextIds))
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case ExtensionBlocks(extension) if discardNextBlocks =>
      discardNextBlocks = false
      log.debug(s"${id(ctx)} discarding just-loaded ${extension.length} blocks as requested")
    case ExtensionBlocks(extension) if hopefullyNextIds.isEmpty =>
      loadNextPart(ctx, extension)
      log.debug(s"${id(ctx)} Passing extension with ${extension.length} blocks upstream")
      super.channelRead(ctx, msg)
    case ExtensionBlocks(extension) =>
      nextExtensionBlocks = extension
    case _ => super.channelRead(ctx, msg)
  }

  private def fmt(tag: String, seq: Seq[_]) = s"$tag:${seq.mkString("\n","\n","\n")}"

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) = msg match {
    case LoadBlockchainExtension(localIds) if hopefullyNextIds == localIds =>
      if (nextExtensionBlocks.isEmpty) {
        log.debug(s"${id(ctx)} Still waiting for extension to load")
        hopefullyNextIds = Seq.empty
      } else {
        log.debug(s"${id(ctx)} Extension already loaded")
        ctx.fireChannelRead(ExtensionBlocks(nextExtensionBlocks))
        loadNextPart(ctx, nextExtensionBlocks)
        nextExtensionBlocks = Seq.empty
      }
    case _: LoadBlockchainExtension if hopefullyNextIds.isEmpty =>
      super.write(ctx, msg, promise)
    case LoadBlockchainExtension(localIds) =>
      val notYetRequestedIds = hopefullyNextIds.dropWhile(_ != localIds.head)
      if (notYetRequestedIds.isEmpty || !hopefullyNextIds.containsSlice(notYetRequestedIds)) {
//        log.debug(s"${fmt("LOCAL IDS", localIds)}${fmt("HOPEFULLY NEXT", hopefullyNextIds)}${fmt("DIFF", notYetRequestedIds)}")
        log.debug(s"${id(ctx)} Got unexpected known block ids, will discard extension once ready")
        discardNextBlocks = true
      }
      hopefullyNextIds = Seq.empty
      nextExtensionBlocks = Seq.empty
      super.write(ctx, msg, promise)
    case _ => super.write(ctx, msg, promise)
  }
}
