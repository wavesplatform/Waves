package com.wavesplatform.network

import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.handler.codec.DecoderException

@Sharable
class ReadErrorHandler(peerDatabase: PeerDatabase) extends ChannelInboundHandlerAdapter with ScorexLogging {
  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = cause match {
    case e: DecoderException =>
      peerDatabase.blacklistAndClose(ctx.channel(), s"Corrupted message frame: $e")

    case e: IndexOutOfBoundsException =>
      peerDatabase.blacklistAndClose(ctx.channel(), s"Index out of bounds: $e")
  }
}
