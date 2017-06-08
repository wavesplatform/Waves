package com.wavesplatform.it.network

import java.net.{InetSocketAddress, NetworkInterface}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import com.wavesplatform.Version
import com.wavesplatform.mining.Miner
import com.wavesplatform.settings._
import com.wavesplatform.state2.reader.StateReader
import io.netty.bootstrap.{Bootstrap, ServerBootstrap}
import io.netty.channel._
import io.netty.channel.group.{ChannelGroup, ChannelMatchers}
import io.netty.channel.local.{LocalAddress, LocalChannel, LocalServerChannel}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import scorex.network.message.{BasicMessagesRepo, MessageSpec}
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}
import scorex.wallet.Wallet

import scala.collection.JavaConverters._
import scala.concurrent.duration._

class NetworkServer(
    chainId: Char,
    settings: WavesSettings,
    allChannels: ChannelGroup,
    peerInfo: ConcurrentHashMap[Channel, PeerInfo]) extends ScorexLogging {

  private val bossGroup = new NioEventLoopGroup()
  private val workerGroup = new NioEventLoopGroup()
  private val handshake =
    Handshake(Constants.ApplicationName + chainId, Version.VersionTuple, settings.networkSettings.nodeName,
      settings.networkSettings.nonce, settings.networkSettings.declaredAddress)

  private val specs: Map[Byte, MessageSpec[_ <: AnyRef]] = (BasicMessagesRepo.specs ++ TransactionalMessagesRepo.specs).map(s => s.messageCode -> s).toMap

  private val address = new LocalAddress("local-events-channel")

  private val localClientGroup = new DefaultEventLoopGroup()
  private val localClientChannel = new Bootstrap()
    .group(localClientGroup)
    .channel(classOf[LocalChannel])
    .handler(new ChannelInitializer[LocalChannel] {
      override def initChannel(ch: LocalChannel) = {}
    })
    .connect(address)
    .channel()

  log.info(s"${id(localClientChannel)} Local channel opened")

  private val peerUniqueness = new ConcurrentHashMap[PeerKey, Channel]()

  private val channels = new ConcurrentHashMap[InetSocketAddress, Channel]

  private val clientHandshakeHandler =
    new HandshakeHandler.Client(handshake, peerInfo, peerUniqueness, _ => ())

  private val bootstrap = new Bootstrap()
    .group(workerGroup)
    .channel(classOf[NioSocketChannel])
    .handler(new LegacyChannelInitializer(clientHandshakeHandler))

  def connect(remoteAddress: InetSocketAddress): Unit =
    channels.computeIfAbsent(remoteAddress, _ => {
      val chan = bootstrap.connect(remoteAddress).channel()
      allChannels.add(chan)
      log.debug(s"${id(chan)} Connecting to $remoteAddress")
      chan.closeFuture().addListener { (chf: ChannelFuture) =>
        log.debug(s"${id(chf.channel)} Connection to $remoteAddress closed")
        allChannels.remove(chf.channel())
        channels.remove(remoteAddress, chf.channel())
      }
      chan
    })

  def writeToLocalChannel(message: AnyRef): Unit = localClientChannel.writeAndFlush(message)

  def shutdown(): Unit = try {
    log.debug("Unbound server")
    allChannels.close().await()
    log.debug("Closed all channels")
  } finally {
    workerGroup.shutdownGracefully()
    bossGroup.shutdownGracefully()
    localClientGroup.shutdownGracefully()
  }
}
