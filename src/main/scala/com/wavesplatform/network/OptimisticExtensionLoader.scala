package com.wavesplatform.network

import com.wavesplatform.state2.ByteStr
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import scorex.block.Block
import scorex.utils.ScorexLogging

class OptimisticExtensionLoader extends ChannelDuplexHandler with ScorexLogging {

  private var hopefullyNextIds = Seq.empty[ByteStr]
  private var requestedLocalIds = Seq.empty[ByteStr]
  private var nextExtensionBlocks = Option.empty[ExtensionBlocks]

  private def loadNextPart(ctx: ChannelHandlerContext, blocks: Seq[Block]): Unit = if (blocks.size > 1) {
    requestedLocalIds = Seq.empty
    hopefullyNextIds = blocks.view.map(_.uniqueId).reverseIterator.take(100).toSeq
    log.debug(s"${id(ctx)} Loading next part, sending ${hopefullyNextIds.size} signatures: ${formatSignatures(hopefullyNextIds)}")
    ctx.writeAndFlush(LoadBlockchainExtension(hopefullyNextIds))
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    // RemoteScoreObserver requested an extension
    case ExtensionBlocks(extension) if requestedLocalIds.nonEmpty =>
      if (extension.isEmpty) {
        stop()
        log.debug(s"${id(ctx)} Blockchain is up to date")
        super.channelRead(ctx, msg)
      } else if (requestedLocalIds.contains(extension.head.reference)) {
        loadNextPart(ctx, extension)
        log.debug(s"${id(ctx)} Passing extension with ${extension.size} blocks upstream")
        super.channelRead(ctx, msg)
      } else {
        log.warn(s"${id(ctx)} Discarding ${extension.size} blocks") // Could lead to hangs
      }

    // We are optimistically requested blocks
    case ext@ExtensionBlocks(extension) if hopefullyNextIds.nonEmpty =>
      if (extension.isEmpty) {
        log.debug(s"${id(ctx)} An optimistically loaded extension has no blocks (blockchain is up to date), stopping the process")
        stop()
      } else {
        nextExtensionBlocks = Some(ext)
        log.debug(s"${id(ctx)} Loaded new extension with ${extension.size} blocks, keeping for now")
      }

    case ExtensionBlocks(extension) =>
      log.debug(s"${id(ctx)} Discarding ${extension.size} blocks")

    case _ => super.channelRead(ctx, msg)
  }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = msg match {
    case LoadBlockchainExtension(Seq()) =>
      log.debug(s"${id(ctx)} Stopping an optimistically loading of extension")
      stop()
      super.write(ctx, msg, promise)

    case LoadBlockchainExtension(localIds) =>
      requestedLocalIds = localIds

      if (hopefullyNextIds.exists(localIds.contains)) {
        nextExtensionBlocks match {
          case None => log.debug(s"${id(ctx)} Still waiting for extension to load")
          case Some(x) =>
            loadNextPart(ctx, x.extension)
            log.debug(s"${id(ctx)} Passing an optimistically loaded extension downstream")
            ctx.fireChannelRead(x)
        }
      } else super.write(ctx, msg, promise)

    case _ => super.write(ctx, msg, promise)
  }

  private def stop(): Unit = {
    requestedLocalIds = Seq.empty
    nextExtensionBlocks = None
    hopefullyNextIds = Seq.empty
  }
}
