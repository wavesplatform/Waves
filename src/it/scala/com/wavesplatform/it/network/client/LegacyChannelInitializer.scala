package com.wavesplatform.it.network.client

import com.wavesplatform.network.{HandshakeDecoder, HandshakeHandler, HandshakeTimeoutHandler, LegacyFrameCodec}
import io.netty.channel.ChannelInitializer
import io.netty.channel.socket.SocketChannel
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}

import scala.concurrent.duration._

class LegacyChannelInitializer(handshakeHandler: HandshakeHandler) extends ChannelInitializer[SocketChannel] {
  override def initChannel(ch: SocketChannel): Unit =
  ch.pipeline()
    .addLast(
      new HandshakeDecoder,
      new HandshakeTimeoutHandler(1.minute),
      handshakeHandler,
      new LengthFieldPrepender(4),
      new LengthFieldBasedFrameDecoder(1024*1024, 0, 4, 0, 4),
      new LegacyFrameCodec)
}
