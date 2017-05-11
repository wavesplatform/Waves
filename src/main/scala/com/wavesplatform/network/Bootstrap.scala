package com.wavesplatform.network

import io.netty.bootstrap.ServerBootstrap
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel

class UnconfirmedTransactionHandler extends ChannelInboundHandlerAdapter

class ServerChannelInitializer extends ChannelInitializer[SocketChannel] {
  override def initChannel(ch: SocketChannel): Unit = {
    ch.pipeline()
      .addLast(new InboundConnectionFilter)
      .addLast(HandshakeDecoder.Name, HandshakeDecoder)
      .addLast(HandshakeTimeoutHandler.Name, new HandshakeTimeoutHandler)
      .addLast(HandshakeHandler.Name, new HandshakeHandler(???))
  }
}

class Bootstrap {
  def zzz(): Unit = {
    val bossGroup = new NioEventLoopGroup()
    val workerGroup = new NioEventLoopGroup()
    val b = new ServerBootstrap()
      .group(bossGroup, workerGroup)
      .channel(classOf[NioServerSocketChannel])
      .childHandler(new ServerChannelInitializer)

    val f = b.bind(6863)
  }
}
