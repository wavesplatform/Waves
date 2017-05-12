package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import java.util.stream.Collectors

import com.wavesplatform.Version
import com.wavesplatform.settings.{Constants, NetworkSettings}
import io.netty.bootstrap.{Bootstrap, ServerBootstrap}
import io.netty.channel._
import io.netty.channel.group.DefaultChannelGroup
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import io.netty.util.concurrent.GlobalEventExecutor
import scorex.network.Handshake
import scorex.utils.ScorexLogging

class ServerChannelInitializer(handshake: Handshake)
  extends ChannelInitializer[SocketChannel] {
  override def initChannel(ch: SocketChannel): Unit = {
    ch.pipeline()
      .addLast(new InboundConnectionFilter)
      .addLast(HandshakeDecoder.Name, new HandshakeDecoder)
      .addLast(HandshakeTimeoutHandler.Name, new HandshakeTimeoutHandler)
  }
}

class ClientChannelInitializer(handshake: Handshake, connections: ConcurrentHashMap[(InetAddress, Long), Channel])
  extends ChannelInitializer[SocketChannel] {
  override def initChannel(ch: SocketChannel): Unit = {
    ch.pipeline()
      .addLast(HandshakeDecoder.Name, new HandshakeDecoder)
      .addLast(HandshakeTimeoutHandler.Name, new HandshakeTimeoutHandler)
      .addLast(ClientHandshakeHandler.Name, new ClientHandshakeHandler(handshake, connections))
  }
}

class Network(chainId: Char, settings: NetworkSettings) extends ScorexLogging {
  private val bossGroup = new NioEventLoopGroup()
  private val workerGroup = new NioEventLoopGroup()
  private val handshake =
    Handshake(Constants.ApplicationName + chainId, Version.VersionTuple, settings.nodeName, settings.nonce, settings.declaredAddress)

  private val allConnectedPeers = new ConcurrentHashMap[(InetAddress, Long), Channel]
  // todo: replace with a better event executor
  private val outgoingChannelGroup = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)

  private val knownPeers = settings.knownPeers.map(inetSocketAddress(_, 6863)).toSet

  private def connectedPeerAddresses =
    allConnectedPeers.keySet.stream.map[InetAddress](a => a._1).collect(Collectors.toSet())
  private def randomKnownPeer: Option[InetSocketAddress] =
    knownPeers.filterNot(p => connectedPeerAddresses.contains(p.getAddress)).headOption

  workerGroup.scheduleWithFixedDelay((() => {
    log.debug("Attempting to connect to a peer")
    val inactiveConnections = outgoingChannelGroup.stream().filter(!_.isActive).collect(Collectors.toSet())
    inactiveConnections.forEach(outgoingChannelGroup.remove _)

    if (outgoingChannelGroup.size() < settings.maxConnections) {
      randomKnownPeer.foreach(connect)
    }
  }): Runnable, 1, 5, TimeUnit.SECONDS)

  private val bootstrap = new Bootstrap()
    .group(workerGroup)
    .channel(classOf[NioSocketChannel])
    .handler(new ClientChannelInitializer(handshake, allConnectedPeers))

  def bind(): ChannelFuture = {
    val b = new ServerBootstrap()
      .group(bossGroup, workerGroup)
      .channel(classOf[NioServerSocketChannel])
      .childHandler(new ServerChannelInitializer(handshake))

    b.bind(6865)
  }

  private def connect(remoteAddress: InetSocketAddress): ChannelFuture = {
    log.debug(s"Connecting to $remoteAddress")
    val f = bootstrap.connect(remoteAddress)
    outgoingChannelGroup.add(f.channel())
    f
  }
}
