package com.wavesplatform.it.network

import io.netty.channel.ChannelInitializer
import io.netty.channel.socket.SocketChannel
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}

class LegacyChannelInitializer(handshakeHandler: HandshakeHandler) extends ChannelInitializer[SocketChannel] {
  override def initChannel(ch: SocketChannel): Unit =
  ch.pipeline()
    .addLast(
      new HandshakeDecoder,
      new HandshakeTimeoutHandler,
      handshakeHandler,
      new LengthFieldPrepender(4),
      new LengthFieldBasedFrameDecoder(1024*1024, 0, 4, 0, 4),
      new LegacyFrameCodec)
}
