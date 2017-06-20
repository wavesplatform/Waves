package com.wavesplatform.network

import com.wavesplatform.state2.ByteStr
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.util.concurrent.ScheduledFuture
import scorex.block.Block
import scorex.transaction.History
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

class ExtensionBlocksLoader(
    history: History,
    blockSyncTimeout: FiniteDuration,
    blacklister: Blacklister) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private var pendingSignatures = Map.empty[ByteStr, Int]
  private var targetExtensionIds = Option.empty[ExtensionIds]
  private val blockBuffer = mutable.TreeMap.empty[Int, Block]
  private var currentTimeout = Option.empty[ScheduledFuture[_]]

  private def cancelTimeout(): Unit = {
    currentTimeout.foreach(_.cancel(false))
    currentTimeout = None
  }

  override def channelInactive(ctx: ChannelHandlerContext) = {
    cancelTimeout()
    super.channelInactive(ctx)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case xid@ExtensionIds(_, newIds) if pendingSignatures.isEmpty =>
      if (newIds.nonEmpty) {
        targetExtensionIds = Some(xid)
        pendingSignatures = newIds.zipWithIndex.toMap
        cancelTimeout()
        currentTimeout = Some(ctx.executor().schedule(blockSyncTimeout) {
          if (targetExtensionIds.contains(xid)) {
            log.warn(s"${id(ctx)} Timeout loading blocks")
            blacklister.blacklist(ctx.channel())
          }
        })
        newIds.foreach(s => ctx.write(GetBlock(s)))
        ctx.flush()
      } else {
        log.debug(s"${id(ctx)} No new blocks to load")
      }

    case b: Block if pendingSignatures.contains(b.uniqueId) =>
      blockBuffer += pendingSignatures(b.uniqueId) -> b
      pendingSignatures -= b.uniqueId
      if (pendingSignatures.isEmpty) {
        cancelTimeout()
        log.debug(s"${id(ctx)} Loaded all blocks, doing a pre-check")

        val newBlocks = blockBuffer.values.toSeq

        for (tids <- targetExtensionIds) {
          if (tids.lastCommonId != newBlocks.head.reference) {
            log.warn(s"${id(ctx)} Extension head reference ${newBlocks.head.reference} differs from last common block id ${tids.lastCommonId}")
            blacklister.blacklist(ctx.channel())
          } else if (!newBlocks.sliding(2).forall {
              case Seq(b1, b2) => b1.uniqueId == b2.reference
              case _ => true
            }) {
            log.warn(s"${id(ctx)} Extension blocks are not contiguous, pre-check failed")
            blacklister.blacklist(ctx.channel())
          } else {
            newBlocks.par.find(!_.signatureValid) match {
              case Some(invalidBlock) =>
                log.warn(s"${id(ctx)} Got block ${invalidBlock.uniqueId} with invalid signature")
                blacklister.blacklist(ctx.channel())
              case None =>
                log.debug(s"${id(ctx)} Chain is valid, pre-check passed")
                ctx.fireChannelRead(ExtensionBlocks(newBlocks))
            }
          }
        }

        targetExtensionIds = None
        blockBuffer.clear()
      }

    case _: ExtensionIds =>
      log.warn(s"${id(ctx)} Received unexpected extension ids while loading blocks, ignoring")
    case _ => super.channelRead(ctx, msg)
  }
}
