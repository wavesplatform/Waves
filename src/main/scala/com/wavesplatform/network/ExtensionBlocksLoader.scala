package com.wavesplatform.network

import com.wavesplatform.state2.EqByteArray
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.util.concurrent.ScheduledFuture
import scorex.block.Block
import scorex.crypto.encode.Base58.encode
import scorex.transaction.History
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

class ExtensionBlocksLoader(history: History, blockSyncTimeout: FiniteDuration)
    extends ChannelInboundHandlerAdapter with ScorexLogging {
  private var pendingSignatures = Map.empty[EqByteArray, Int]
  private var targetExtensionIds = Option.empty[ExtensionIds]
  private val blockBuffer = mutable.TreeMap.empty[Int, Block]
  private var currentTimeout = Option.empty[ScheduledFuture[_]]

  private def cancelTimeout(): Unit = {
    currentTimeout.foreach(_.cancel(false))
    currentTimeout = None
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case xid@ExtensionIds(_, newIds) if pendingSignatures.isEmpty =>
      targetExtensionIds = Some(xid)
      pendingSignatures = newIds.map(EqByteArray).zipWithIndex.toMap
      cancelTimeout()
      currentTimeout = Some(ctx.executor().schedule(blockSyncTimeout) {
        if (targetExtensionIds.contains(xid)) {
          log.warn(s"Timeout loading signatures from ${ctx.channel().id().asShortText()}")
          // todo: blacklist?
        }
      })
      newIds.foreach(s => ctx.write(GetBlock(s)))
      ctx.flush()

    case b: Block if pendingSignatures.contains(EqByteArray(b.uniqueId)) =>
      val signature = EqByteArray(b.uniqueId)
      blockBuffer += pendingSignatures(signature) -> b
      pendingSignatures -= signature
      if (pendingSignatures.isEmpty) {
        cancelTimeout()
        log.debug("Loaded all blocks, doing a pre-check")

        val newBlocks = blockBuffer.values.toSeq

        for (tids <- targetExtensionIds) {
          if (!tids.lastCommonId.sameElements(newBlocks.head.reference)) {
            log.warn(s"Extension head reference ${encode(newBlocks.head.reference)} differs from last common block id ${encode(tids.lastCommonId)}")
            // todo: blacklist?
          } else if (!newBlocks.sliding(2).forall {
              case Seq(b1, b2) => b1.uniqueId.sameElements(b2.reference)
              case _ => true
            }) {
            log.warn("Extension blocks are not contiguous, pre-check failed")
            // todo: blacklist?
          } else {
            val localScore = history.score()
            val forkScore = newBlocks.view.map(_.blockScore).foldLeft(history.scoreOf(tids.lastCommonId))(_ + _)

            if (forkScore <= localScore) {
              log.debug(s"Fork score $forkScore is not higher than local score $localScore, pre-check failed")
            } else {
              log.debug(s"Fork score $forkScore is higher than local score $localScore, pre-check passed")
              ctx.fireChannelRead(ExtensionBlocks(newBlocks))
            }
          }
        }

        targetExtensionIds = None
        blockBuffer.clear()
      }

    case Signatures(sigs) =>
      log.warn(s"Received unexpected extension ids from ${ctx.channel().id().asShortText()} while loading blocks, ignoring")
    case _ => super.channelRead(ctx, msg)
  }
}
