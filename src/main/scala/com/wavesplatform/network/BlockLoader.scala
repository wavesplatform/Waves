package com.wavesplatform.network

import java.util

import com.wavesplatform.state2.EqByteArray
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.util.concurrent.ScheduledFuture
import scorex.block.Block
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

class BlockLoader(blockSyncTimeout: FiniteDuration) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private var pendingSignatures = Map.empty[EqByteArray, Int]
  private var targetSignatures = Seq.empty[EqByteArray]
  private val blockBuffer = mutable.TreeMap.empty[Int, Block]
  private var currentTimeout = Option.empty[ScheduledFuture[_]]

  private def cancelTimeout(): Unit = {
    currentTimeout.foreach(_.cancel(false))
    currentTimeout = None
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case Signatures(newSignatures) if pendingSignatures.isEmpty =>
      val newTargetSignatures = newSignatures.map(EqByteArray)
      targetSignatures = newTargetSignatures
      pendingSignatures = targetSignatures.zipWithIndex.toMap
      log.debug(s"Ensure capacity ${targetSignatures.length}, actual size ${blockBuffer.size}")
      cancelTimeout()
      currentTimeout = Some(ctx.executor().schedule(blockSyncTimeout) {
        if (newTargetSignatures == targetSignatures) {
          log.warn(s"Timeout loading signatures from ${ctx.channel().id().asShortText()}")
          // todo: blacklist
        }
      })
      newSignatures.foreach(s => ctx.write(GetBlock(s)))
      ctx.flush()

    case b: Block =>
      val signature = EqByteArray(b.uniqueId)
      if (pendingSignatures.contains(signature)) {
        blockBuffer += pendingSignatures(signature) -> b
        pendingSignatures -= signature
        if (pendingSignatures.isEmpty) {
          cancelTimeout()
          log.debug("Loaded all blocks, doing a pre-check")

          val newBlocks = blockBuffer.values.toSeq
          if (newBlocks.sliding(2).forall {
            case Seq(b1, b2) => util.Arrays.equals(b1.uniqueId, b2.reference)
            case _ => true
          }) {
            log.debug(s"Extension contains no gaps, will attempt to add ${newBlocks.length} blocks")
            // todo: propagate extension
          } else {
            log.warn(s"Extension contains gaps")
            // todo: block peer
          }

          targetSignatures = Seq.empty
          blockBuffer.clear()
        }
      } else {
        super.channelRead(ctx, msg)
      }


    case Signatures(sigs) =>
      log.warn(s"For some reason received unexpected extension from ${ctx.channel().id().asShortText()}, ignoring")
    case _ => super.channelRead(ctx, msg)
  }
}
