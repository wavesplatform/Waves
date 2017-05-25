package com.wavesplatform.network

import java.util.concurrent.atomic.AtomicBoolean

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.network.TransactionalMessagesRepo.TransactionMessageSpec
import scorex.utils.ScorexLogging

@Sharable
class DiscardingHandler extends ChannelInboundHandlerAdapter with ScorexLogging {
  private val blockchainIsOutdated = new AtomicBoolean(true)

  def enable(): Unit = blockchainIsOutdated.compareAndSet(false, true)
  def disable(): Unit = blockchainIsOutdated.compareAndSet(true, false)

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case RawBytes(code, _) if code == TransactionMessageSpec.messageCode && blockchainIsOutdated.get() =>
      log.trace(s"Discarding incoming message $code from ${ctx.channel().id().asShortText()}")
    case _ => super.channelRead(ctx, msg)
  }
}
