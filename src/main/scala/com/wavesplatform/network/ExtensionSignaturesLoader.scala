package com.wavesplatform.network

import java.util.concurrent.ScheduledFuture

import com.wavesplatform.state2.ByteStr
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration

class ExtensionSignaturesLoader(syncTimeout: FiniteDuration, peerDatabase: PeerDatabase)
  extends ChannelDuplexHandler with ScorexLogging {

  private var currentTimeout = Option.empty[ScheduledFuture[Unit]]
  private var lastKnownSignatures = Seq.empty[ByteStr]

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case s: Signatures =>
      val (known, unknown) = s.signatures.span(id => lastKnownSignatures.contains(id))
      currentTimeout.foreach(_.cancel(true))
      currentTimeout = None
      known.lastOption.foreach { lastKnown =>
        log.debug(s"${id(ctx)} Got extension with ${known.length}/${s.signatures.length} known signatures")
        ctx.fireChannelRead(ExtensionIds(lastKnown, unknown))
      }
    case _ => super.channelRead(ctx, msg)
  }

  override def channelInactive(ctx: ChannelHandlerContext) = {
    currentTimeout.foreach(_.cancel(false))
    currentTimeout = None
  }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) = msg match {
    case LoadBlockchainExtension(sigs) if currentTimeout.isEmpty =>
      lastKnownSignatures = sigs

      log.debug(s"${id(ctx)} Loading extension, last ${sigs.length} are [${sigs.head}..${sigs.last}]")

      currentTimeout = Some(ctx.executor().schedule(syncTimeout) {
        if (currentTimeout.nonEmpty && ctx.channel().isActive) {
          log.warn(s"${id(ctx)} Timeout expired while loading extension")
          peerDatabase.blacklistAndClose(ctx.channel())
        }
      })

      ctx.writeAndFlush(GetSignatures(sigs), promise)

    case LoadBlockchainExtension(sigs) =>
      log.debug(s"${id(ctx)} Received request to load signatures while waiting for extension, ignoring for now")
      promise.setSuccess()

    case _ => super.write(ctx, msg, promise)
  }
}
