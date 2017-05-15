package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import java.util.stream.Collectors

import com.wavesplatform.Version
import com.wavesplatform.settings.{Constants, WavesSettings}
import io.netty.bootstrap.{Bootstrap, ServerBootstrap}
import io.netty.channel._
import io.netty.channel.group.DefaultChannelGroup
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import io.netty.handler.codec.LengthFieldBasedFrameDecoder
import io.netty.util.concurrent.GlobalEventExecutor
import scorex.network.message.{BasicMessagesRepo, MessageSpec}
import scorex.network.{Handshake, TransactionalMessagesRepo}
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

class ClientChannelInitializer(
    handshake: Handshake,
    scoreObserver: ScoreObserver,
    connections: ConcurrentHashMap[PeerKey, Channel])
  extends ChannelInitializer[SocketChannel] {

  private val specs: Map[Byte, MessageSpec[_ <: AnyRef]] = (BasicMessagesRepo.specs ++ TransactionalMessagesRepo.specs).map(s => s.messageCode -> s).toMap

  override def initChannel(ch: SocketChannel): Unit = {
    ch.pipeline()
      .addLast(HandshakeDecoder.Name, new HandshakeDecoder)
      .addLast(HandshakeTimeoutHandler.Name, new HandshakeTimeoutHandler)
      .addLast(ClientHandshakeHandler.Name, new ClientHandshakeHandler(handshake, connections))
      .addLast(new LengthFieldBasedFrameDecoder(1024*1024, 0, 4, 0, 4))
      .addLast(new MessageCodec(specs))
      .addLast(scoreObserver)
  }
}

class Network(chainId: Char, settings: WavesSettings) extends ScorexLogging {
  private val bossGroup = new NioEventLoopGroup()
  private val workerGroup = new NioEventLoopGroup()
  private val handshake =
    Handshake(Constants.ApplicationName + chainId, Version.VersionTuple, settings.networkSettings.nodeName,
      settings.networkSettings.nonce, settings.networkSettings.declaredAddress)

  private val allConnectedPeers = new ConcurrentHashMap[PeerKey, Channel]
  // todo: replace with a better event executor
  private val outgoingChannelGroup = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)

  private val knownPeers = settings.networkSettings.knownPeers.map(inetSocketAddress(_, 6863)).toSet

  private val scoreObserver = new ScoreObserver(settings.synchronizationSettings.scoreTTL)

  private def connectedPeerAddresses =
    allConnectedPeers.keySet.stream.map[InetAddress](pk => pk.host).collect(Collectors.toSet())
  private def randomKnownPeer: Option[InetSocketAddress] =
    knownPeers.filterNot(p => connectedPeerAddresses.contains(p.getAddress)).headOption

  private val repeatedConnectionAttempt = workerGroup.scheduleWithFixedDelay((() => {
    log.debug("Attempting to connect to a peer")
    val inactiveConnections = outgoingChannelGroup.stream().filter(!_.isActive).collect(Collectors.toSet())
    inactiveConnections.forEach(outgoingChannelGroup.remove _)

    if (outgoingChannelGroup.size() < settings.networkSettings.maxConnections) {
      randomKnownPeer.foreach(connect)
    }
  }): Runnable, 1, 5, TimeUnit.SECONDS)

  private val bootstrap = new Bootstrap()
    .group(workerGroup)
    .channel(classOf[NioSocketChannel])
    .handler(new ClientChannelInitializer(handshake, scoreObserver, allConnectedPeers))

  private val bindingFuture = {
    val b = new ServerBootstrap()
      .group(bossGroup, workerGroup)
      .channel(classOf[NioServerSocketChannel])
      .childHandler(new ServerChannelInitializer(handshake))

    b.bind(6865)
  }

  private def connect(remoteAddress: InetSocketAddress): ChannelFuture = {
    val f = bootstrap.connect(remoteAddress)
    log.debug(s"Connecting to $remoteAddress ${f.channel().id().asShortText()}")
    outgoingChannelGroup.add(f.channel())
    f
  }

  def shutdown(): Unit = try {
    repeatedConnectionAttempt.cancel(true)
    outgoingChannelGroup.close().await()
    bindingFuture.channel().closeFuture().await()
  } finally {
    workerGroup.shutdownGracefully()
    bossGroup.shutdownGracefully()
  }
}
