package com.wavesplatform.network

import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.metrics.LatencyHistogram
import com.wavesplatform.state2.ByteStr
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.util.concurrent.ScheduledFuture
import kamon.Kamon
import kamon.metric.instrument
import scorex.block.Block
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.duration.FiniteDuration

class ExtensionBlocksLoader(blockSyncTimeout: FiniteDuration,
                            peerDatabase: PeerDatabase,
                            history: NgHistory,
                            invalidBlocks: InvalidBlockStorage,
                            blockchainReadiness: AtomicBoolean) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private var pendingSignatures = Map.empty[ByteStr, Int]
  private var targetExtensionIds = Option.empty[ExtensionIds]
  private val blockBuffer = mutable.TreeMap.empty[Int, Block]
  private var blacklistingScheduledFuture = Option.empty[ScheduledFuture[_]]
  private val extensionsFetchingTimeStats = new LatencyHistogram(Kamon.metrics.histogram("extensions-fetching-time", instrument.Time.Milliseconds))

  private def cancelBlacklist(): Unit = {
    blacklistingScheduledFuture.foreach(_.cancel(false))
    blacklistingScheduledFuture = None
  }

  private def blacklistAfterTimeout(ctx: ChannelHandlerContext): Unit = {
    cancelBlacklist()
    blacklistingScheduledFuture = Some(ctx.executor().schedule(blockSyncTimeout) {
      peerDatabase.blacklistAndClose(ctx.channel(), "Timeout loading blocks")
    })
  }

  override def channelInactive(ctx: ChannelHandlerContext): Unit = {
    cancelBlacklist()
    super.channelInactive(ctx)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case xid@ExtensionIds(_, newIds) if pendingSignatures.isEmpty =>
      val requestingIds = newIds.filterNot(history.contains)
      if (requestingIds.nonEmpty) {
        log.trace(s"${id(ctx)} About to load ${requestingIds.size} blocks")
        targetExtensionIds = Some(xid)
        pendingSignatures = newIds.zipWithIndex.toMap
        blacklistAfterTimeout(ctx)
        extensionsFetchingTimeStats.start()
        newIds.foreach(s => ctx.write(GetBlock(s)))
        ctx.flush()
      } else {
        log.debug(s"${id(ctx)} No new blocks to load")
        ctx.fireChannelRead(ExtensionBlocks(Seq.empty))
      }
    case b: Block if pendingSignatures.contains(b.uniqueId) =>
      log.trace(s"${id(ctx)} Received expected block ${b.uniqueId}")
      blockBuffer += pendingSignatures(b.uniqueId) -> b
      pendingSignatures -= b.uniqueId
      blacklistAfterTimeout(ctx)
      if (pendingSignatures.isEmpty) {
        cancelBlacklist()
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
            log.trace(s"${id(ctx)} Checking extension block signatures")
            val pnewBlocks = newBlocks.par
            pnewBlocks.tasksupport = new ForkJoinTaskSupport()
            pnewBlocks.find(_.signaturesValid().isLeft) match {
              case Some(invalidBlock) =>
                invalidBlocks.add(invalidBlock.uniqueId)
                peerDatabase.blacklistAndClose(ctx.channel(),s"Got block $invalidBlock with invalid signature")
              case None =>
                log.debug(s"${id(ctx)} Successfully loaded extension blocks ${formatSignatures(newBlocks.map(_.signerData.signature))}")
                ctx.fireChannelRead(ExtensionBlocks(newBlocks))
            }
          }
        }

        targetExtensionIds = None
        blockBuffer.clear()
      }

    case _: Block =>
      if (blockchainReadiness.get) {
        log.trace(s"${id(ctx)} Discarding block because blockchain is too old")
      } else {
        log.trace(s"${id(ctx)} Passing block upstream: ${blockchainReadiness.get()}")
      }

    case ExtensionIds(_, sigs) =>
      log.warn(s"${id(ctx)} Waiting for ${pendingSignatures.size} more blocks(s), ignoring new signatures ${formatSignatures(sigs)}")
    case _ => super.channelRead(ctx, msg)
  }
}
