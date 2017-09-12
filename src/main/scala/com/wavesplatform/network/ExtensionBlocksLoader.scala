package com.wavesplatform.network

import com.wavesplatform.metrics.LatencyHistogram
import com.wavesplatform.state2.ByteStr
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.util.concurrent.ScheduledFuture
import kamon.Kamon
import kamon.metric.instrument
import scorex.block.Block
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

class ExtensionBlocksLoader(
    blockSyncTimeout: FiniteDuration,
    peerDatabase: PeerDatabase) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private var pendingSignatures = Map.empty[ByteStr, Int]
  private var targetExtensionIds = Option.empty[ExtensionIds]
  private val blockBuffer = mutable.TreeMap.empty[Int, Block]
  private var currentTimeout = Option.empty[ScheduledFuture[_]]
  private val extensionsFetchingTimeStats = new LatencyHistogram(Kamon.metrics.histogram("extensions-fetching-time", instrument.Time.Milliseconds))

  private def cancelTimeout(): Unit = {
    currentTimeout.foreach(_.cancel(false))
    currentTimeout = None
  }

  private def resetTimeout(ctx: ChannelHandlerContext, xid: ExtensionIds): Unit = {
    cancelTimeout()
    currentTimeout = Some(ctx.executor().schedule(blockSyncTimeout) {
      if (targetExtensionIds.contains(xid)) {
        peerDatabase.blacklistAndClose(ctx.channel(), "Timeout loading blocks")
      }
    })
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
          resetTimeout(ctx, xid)
          extensionsFetchingTimeStats.start()
          newIds.foreach(s => ctx.write(GetBlock(s)))
          ctx.flush()
        } else {
          log.debug(s"${id(ctx)} No new blocks to load")
          ctx.fireChannelRead(ExtensionBlocks(Seq.empty))
        }
    case b: Block if pendingSignatures.contains(b.uniqueId) =>
      blockBuffer += pendingSignatures(b.uniqueId) -> b
      pendingSignatures -= b.uniqueId
      if (pendingSignatures.isEmpty) {
        cancelTimeout()
        extensionsFetchingTimeStats.record()
        log.trace(s"${id(ctx)} Loaded all blocks, doing a pre-check")

        val newBlocks = blockBuffer.values.toSeq

        for (tids <- targetExtensionIds) {
          if (tids.lastCommonId != newBlocks.head.reference) {
            peerDatabase.blacklistAndClose(ctx.channel(),s"Extension head reference ${newBlocks.head.reference} differs from last common block id ${tids.lastCommonId}")
          } else if (!newBlocks.sliding(2).forall {
              case Seq(b1, b2) => b1.uniqueId == b2.reference
              case _ => true
            }) {
            peerDatabase.blacklistAndClose(ctx.channel(),"Extension blocks are not contiguous, pre-check failed")
          } else {
            newBlocks.par.find(!_.signatureValid) match {
              case Some(invalidBlock) =>
                peerDatabase.blacklistAndClose(ctx.channel(),s"Got block ${invalidBlock.uniqueId} with invalid signature")
              case None =>
                log.trace(s"${id(ctx)} Chain is valid, pre-check passed")
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
