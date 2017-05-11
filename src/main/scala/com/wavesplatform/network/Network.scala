package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import java.util.stream.Collectors

import io.netty.bootstrap.{Bootstrap, ServerBootstrap}
import io.netty.channel._
import io.netty.channel.group.DefaultChannelGroup
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import io.netty.util.concurrent.GlobalEventExecutor
import scorex.network.Handshake

class UnconfirmedTransactionHandler extends ChannelInboundHandlerAdapter

class ServerChannelInitializer(handshake: Handshake) extends ChannelInitializer[SocketChannel] {
  override def initChannel(ch: SocketChannel): Unit = {
    ch.pipeline()
      .addLast(new InboundConnectionFilter)
      .addLast(HandshakeDecoder.Name, HandshakeDecoder)
      .addLast(HandshakeTimeoutHandler.Name, new HandshakeTimeoutHandler)
      .addLast(HandshakeHandler.Name, new HandshakeHandler(handshake))
  }
}

class ClientChannelInitializer(handshake: Handshake) extends ChannelInitializer[SocketChannel] {
  override def initChannel(ch: SocketChannel): Unit = {
    ch.pipeline()
      .addLast(HandshakeDecoder.Name, HandshakeDecoder)
      .addLast(HandshakeTimeoutHandler.Name, new HandshakeTimeoutHandler)
      .addLast(HandshakeHandler.Name, new HandshakeHandler(handshake))
  }
}

class Network(handshake: Handshake, maxOutgoingConnections: Int) {
  private val bossGroup = new NioEventLoopGroup()
  private val workerGroup = new NioEventLoopGroup()

  private val allConnectedPeers = new ConcurrentHashMap[(InetAddress, Int), Channel]
  // todo: replace with a better event executor
  private val outgoingConnections = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)

  private def randomKnownPeer: InetSocketAddress = ???

  workerGroup.scheduleWithFixedDelay((() => {
    val inactiveConnections = outgoingConnections.stream().filter(!_.isActive).collect(Collectors.toSet())
    inactiveConnections.forEach(outgoingConnections.remove _)

    if (outgoingConnections.size() < maxOutgoingConnections) {
      connect(randomKnownPeer)
    }
  }): Runnable, 1, 5, TimeUnit.SECONDS)

  private val bootstrap = new Bootstrap()
    .group(workerGroup)
    .channel(classOf[NioSocketChannel])
    .handler(new ClientChannelInitializer(handshake))

  def bind(): ChannelFuture = {
    val b = new ServerBootstrap()
      .group(bossGroup, workerGroup)
      .channel(classOf[NioServerSocketChannel])
      .childHandler(new ServerChannelInitializer(handshake))

    b.bind(6863)
  }

  private def connect(remoteAddress: InetSocketAddress): ChannelFuture = {
    val f = bootstrap.connect(remoteAddress)
    outgoingConnections.add(f.channel())
    f
  }
}
