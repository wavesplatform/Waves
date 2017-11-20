package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext}
import monix.reactive.Observable
import scorex.utils.ScorexLogging

@Sharable
class DiscardingHandler(blockchainReadiness : Observable[Boolean]) extends ChannelDuplexHandler with ScorexLogging {
  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
//    case RawBytes(code, _) if code == TransactionMessageSpec.messageCode && blockchainReadiness.lastOptionL =>
//      log.trace(s"${id(ctx)} Discarding incoming message $code")
    case _ => super.channelRead(ctx, msg)
  }
}
