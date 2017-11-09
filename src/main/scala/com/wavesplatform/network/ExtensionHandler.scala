package com.wavesplatform.network


import com.wavesplatform.metrics.LatencyHistogram
import com.wavesplatform.state2.ByteStr
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import io.netty.util.concurrent.ScheduledFuture
import kamon.Kamon
import kamon.metric.instrument
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.duration.FiniteDuration


class ExtensionHandler(syncTimeout: FiniteDuration,
                       peerDatabase: PeerDatabase, history: NgHistory)
  extends ChannelDuplexHandler with ScorexLogging {

  import ExtensionHandler._

  // ExtensionSignaturesLoader
  private var currentTimeout = Option.empty[ScheduledFuture[Unit]]
  private var lastKnownSignatures = Seq.empty[ByteStr]

  // ExtensionBlocksLoader
  private var pendingSignatures = Map.empty[ByteStr, Int]
  private var targetExtensionIds = Option.empty[ExtensionIds]
  private val blockBuffer = mutable.TreeMap.empty[Int, Block]
  private var blacklistingScheduledFuture = Option.empty[ScheduledFuture[_]]

  // OptimisticExtensionLoader
  private var hopefullyNextIds = Seq.empty[ByteStr]
  private var nextExtensionBlocks = Seq.empty[Block]
  private var discardNextBlocks = false

  private val extensionsFetchingTimeStats = new LatencyHistogram(Kamon.metrics.histogram("extensions-fetching-time", instrument.Time.Milliseconds))

  private def blacklistAfterTimeout(ctx: ChannelHandlerContext): Unit = {
    blacklistingScheduledFuture.foreach(_.cancel(false))
    blacklistingScheduledFuture = Some(ctx.executor().schedule(syncTimeout) {
      peerDatabase.blacklistAndClose(ctx.channel(), "Timeout loading blocks")
    })
  }

  override def channelInactive(ctx: ChannelHandlerContext): Unit = {
    blacklistingScheduledFuture.foreach(_.cancel(false))
    blacklistingScheduledFuture = None
    currentTimeout.foreach(_.cancel(false))
    currentTimeout = None
    super.channelInactive(ctx)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case s: Signatures =>
      val (known, unknown) = s.signatures.span(id => lastKnownSignatures.contains(id))
      currentTimeout.foreach(_.cancel(true))
      currentTimeout = None
      known.lastOption match {
        case None => log.warn(s"Got unknown extensions from ${id(ctx)}: ${s.signatures.map(_.trim).mkString(",")}")
        case Some(lastKnown) =>
          log.debug(s"${id(ctx)} Got extension with ${known.length}/${s.signatures.length} known signatures")
          if (pendingSignatures.isEmpty) {
            val requestingIds = unknown.filterNot(history.contains)
            if (requestingIds.nonEmpty) {
              targetExtensionIds = Some(ExtensionIds(lastKnown, unknown))
              pendingSignatures = unknown.zipWithIndex.toMap
              blacklistAfterTimeout(ctx)
              extensionsFetchingTimeStats.start()
              unknown.foreach(s => ctx.write(GetBlock(s)))
              ctx.flush()
            } else {
              log.debug(s"${id(ctx)} No new blocks to load")
              handleExtensionBlocks(ctx, Seq.empty)
            }
          } else {
            log.warn(s"${id(ctx)} Received unexpected extension ids while loading blocks, ignoring")
          }
      }
    case b: Block if pendingSignatures.contains(b.uniqueId) =>
      blockBuffer += pendingSignatures(b.uniqueId) -> b
      pendingSignatures -= b.uniqueId
      blacklistAfterTimeout(ctx)
      if (pendingSignatures.isEmpty) {
        {
          blacklistingScheduledFuture.foreach(_.cancel(false))
          blacklistingScheduledFuture = None
        }
        extensionsFetchingTimeStats.record()
        log.trace(s"${id(ctx)} Loaded all blocks, doing a pre-check")

        val newBlocks = blockBuffer.values.toSeq

        for (tids <- targetExtensionIds) {
          if (tids.lastCommonId != newBlocks.head.reference) {
            peerDatabase.blacklistAndClose(ctx.channel(), s"Extension head reference ${newBlocks.head.reference} differs from last common block id ${tids.lastCommonId}")
          } else if (!newBlocks.sliding(2).forall {
            case Seq(b1, b2) => b1.uniqueId == b2.reference
            case _ => true
          }) {
            peerDatabase.blacklistAndClose(ctx.channel(), "Extension blocks are not contiguous, pre-check failed")
          } else {
            val pnewBlocks = newBlocks.par
            pnewBlocks.tasksupport = new ForkJoinTaskSupport()
            pnewBlocks.find(_.signaturesValid().isLeft) match {
              case Some(invalidBlock) =>
                peerDatabase.blacklistAndClose(ctx.channel(), s"Got block $invalidBlock with invalid signature")
              case None =>
                log.trace(s"${id(ctx)} Chain is valid, pre-check passed")
                handleExtensionBlocks(ctx, newBlocks)
            }
          }
        }

        targetExtensionIds = None
        blockBuffer.clear()
      }

    case _ => super.channelRead(ctx, msg)
  }

  private def loadNextPart(ctx: ChannelHandlerContext, blocks: Seq[Block]): Unit = if (blocks.size > 1) {
    // Receiving just one block usually means we've reached the end of blockchain. Pre-Netty nodes
    // didn't handle GetSignatures(lastBlockId) message properly, hence the check.
    log.trace(s"${id(ctx)} loading next part")
    hopefullyNextIds = blocks.view.map(_.uniqueId).reverseIterator.take(100).toSeq
    requestIfNotEmpty(ctx, hopefullyNextIds)
  }

  def handleExtensionBlocks(ctx: ChannelHandlerContext, extension: Seq[Block]): Unit = {
    if (discardNextBlocks) {
      discardNextBlocks = false
      log.debug(s"${id(ctx)} discarding just-loaded ${extension.length} blocks as requested")
    } else if (extension.isEmpty) {
      log.debug(s"${id(ctx)} Blockchain is up to date")
      hopefullyNextIds = Seq.empty
      super.channelRead(ctx, ExtensionBlocks(extension))
    }
    else if (hopefullyNextIds.isEmpty) {
      loadNextPart(ctx, extension)
      log.trace(s"${id(ctx)} Passing extension with ${extension.length} blocks upstream")
      super.channelRead(ctx, ExtensionBlocks(extension))
    } else {
      nextExtensionBlocks = extension
    }
  }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) = msg match {
    case LoadBlockchainExtension(localIds) => h0(ctx, promise, localIds)
    case _ => super.write(ctx, msg, promise)
  }


  def h0(ctx: ChannelHandlerContext, promise: ChannelPromise, localIds: Seq[ByteStr]) = {
    if (hopefullyNextIds == localIds) {
      if (nextExtensionBlocks.isEmpty) {
        log.debug(s"${id(ctx)} Still waiting for extension to load")
        hopefullyNextIds = Seq.empty
      } else {
        log.debug(s"${id(ctx)} Extension already loaded")
        ctx.fireChannelRead(ExtensionBlocks(nextExtensionBlocks))
        loadNextPart(ctx, nextExtensionBlocks)
        nextExtensionBlocks = Seq.empty
      }
    } else {
      if (hopefullyNextIds.nonEmpty) {
        val notYetRequestedIds = hopefullyNextIds.dropWhile(_ != localIds.head)
        if (notYetRequestedIds.isEmpty || !hopefullyNextIds.containsSlice(notYetRequestedIds)) {
          discardNextBlocks = nextExtensionBlocks.isEmpty
          log.debug(s"${id(ctx)} Got unexpected known block ids${if (discardNextBlocks) ", will discard extension once ready" else ""}")
        }
        hopefullyNextIds = Seq.empty
        nextExtensionBlocks = Seq.empty
      }
      requestIfNotEmpty(ctx, localIds)
    }
  }

  def requestIfNotEmpty(ctx: ChannelHandlerContext, localIds: Seq[BlockId]): Unit = {
    if (currentTimeout.isEmpty) {
      lastKnownSignatures = localIds
      log.debug(s"${id(ctx)} Loading extension, last ${localIds.length} are ${formatSignatures(localIds)}")
      currentTimeout = Some(ctx.executor().schedule(syncTimeout) {
        if (currentTimeout.nonEmpty && ctx.channel().isActive) {
          peerDatabase.blacklistAndClose(ctx.channel(), "Timeout expired while loading extension")
        }
      })
      ctx.requestSignatures(localIds)
    } else {
      log.debug(s"${id(ctx)} Received request to load signatures while waiting for extension, ignoring for now")
    }
  }
}


object ExtensionHandler {

  implicit class ChannelHandlerContextExt(ctx: ChannelHandlerContext) {
    def requestSignatures(lastIds: Seq[BlockId]): Unit = ctx.writeAndFlush(GetSignatures(lastIds))
  }

}