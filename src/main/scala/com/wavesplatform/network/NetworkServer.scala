package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.ConcurrentHashMap
import java.util.stream.Collectors

import com.wavesplatform.Version
import com.wavesplatform.settings.{Constants, WavesSettings}
import io.netty.bootstrap.{Bootstrap, ServerBootstrap}
import io.netty.channel._
import io.netty.channel.group.{ChannelGroup, ChannelMatchers}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import io.netty.handler.codec.LengthFieldBasedFrameDecoder
import scorex.network.message.{BasicMessagesRepo, MessageSpec}
import scorex.network.{Handshake, TransactionalMessagesRepo}
import scorex.transaction.History
import scorex.utils.ScorexLogging

import scala.concurrent.duration._

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

trait Network {
  def broadcast(message: AnyRef, except: Option[Channel] = None)
  def sendToRandom(message: AnyRef)
}

class NetworkServer(chainId: Char, settings: WavesSettings, history: History, allChannels: ChannelGroup) extends ScorexLogging {
  private val bossGroup = new NioEventLoopGroup()
  private val workerGroup = new NioEventLoopGroup()
  private val handshake =
    Handshake(Constants.ApplicationName + chainId, Version.VersionTuple, settings.networkSettings.nodeName,
      settings.networkSettings.nonce, settings.networkSettings.declaredAddress)

  private val allConnectedPeers = new ConcurrentHashMap[PeerKey, Channel]
  private val knownPeers = settings.networkSettings.knownPeers.map(inetSocketAddress(_, 6863)).toSet
  private val scoreObserver = new ScoreObserver(settings.synchronizationSettings.scoreTTL)

  private def connectedPeerAddresses =
    allConnectedPeers.keySet.stream.map[InetAddress](pk => pk.host).collect(Collectors.toSet())
  private def randomKnownPeer: Option[InetSocketAddress] =
    knownPeers.filterNot(p => connectedPeerAddresses.contains(p.getAddress)).headOption

  private val repeatedConnectionAttempt = workerGroup.scheduleWithFixedDelay(1.second, 5.seconds) {
    val inactiveConnections = allChannels.stream().filter(!_.isActive).collect(Collectors.toSet())
    inactiveConnections.forEach(allChannels.remove _)

    if (allChannels.size() < settings.networkSettings.maxConnections) {
      randomKnownPeer.foreach(connect)
    }
  }

  private val broadcastScrores =
    workerGroup.scheduleWithFixedDelay(5.second, settings.synchronizationSettings.scoreBroadcastInterval) {
      broadcast(history.score())
    }

  private val bootstrap = new Bootstrap()
    .group(workerGroup)
    .channel(classOf[NioSocketChannel])
    .handler(new ClientChannelInitializer(handshake, scoreObserver, allConnectedPeers))

  private val serverChannel = new ServerBootstrap()
    .group(bossGroup, workerGroup)
    .channel(classOf[NioServerSocketChannel])
    .childHandler(new ServerChannelInitializer(handshake))
    .bind(settings.networkSettings.port)
    .channel()

  private def connect(remoteAddress: InetSocketAddress): ChannelFuture = {
    val f = bootstrap.connect(remoteAddress)
    log.debug(s"Connecting to $remoteAddress ${f.channel().id().asShortText()}")
    allChannels.add(f.channel())
    f
  }

  def broadcast(message: AnyRef, except: Option[Channel] = None): Unit = {
    log.debug(s"Broadcasting $message to ${allChannels.size()} channels${except.fold("")(c => s" (except ${c.id().asShortText()})")}")
    allChannels.writeAndFlush(message, except.fold(ChannelMatchers.all())(ChannelMatchers.isNot))
  }

  def shutdown(): Unit = try {
    serverChannel.close().await()
    log.debug("Unbound server")

    repeatedConnectionAttempt.cancel(true)
    allChannels.close().await()
    log.debug("Closed all channels")
  } finally {
    workerGroup.shutdownGracefully()
    bossGroup.shutdownGracefully()
  }
}
