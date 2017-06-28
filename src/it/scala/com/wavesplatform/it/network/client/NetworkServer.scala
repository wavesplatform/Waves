package com.wavesplatform.it.network.client

import java.net.InetSocketAddress
import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.Version
import com.wavesplatform.network.{Handshake, HandshakeHandler, PeerInfo, PeerKey}
import com.wavesplatform.settings._
import io.netty.bootstrap.Bootstrap
import io.netty.channel._
import io.netty.channel.group.ChannelGroup
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import scorex.utils.ScorexLogging

class NetworkServer(
    chainId: Char,
    settings: WavesSettings,
    allChannels: ChannelGroup,
    peerInfo: ConcurrentHashMap[Channel, PeerInfo]) extends ScorexLogging {

  private val bossGroup = new NioEventLoopGroup()
  private val workerGroup = new NioEventLoopGroup()

  private val handshake =
    Handshake(Constants.ApplicationName + chainId, Version.VersionTuple, settings.networkSettings.nodeName,
      settings.networkSettings.nonce, None)

  private val peerUniqueness = new ConcurrentHashMap[PeerKey, Channel]()

  private val channels = new ConcurrentHashMap[InetSocketAddress, Channel]

  private val clientHandshakeHandler =
    new HandshakeHandler.Client(handshake, peerInfo, peerUniqueness, new InMemoryPeerDatabase)

  private val bootstrap = new Bootstrap()
    .group(workerGroup)
    .channel(classOf[NioSocketChannel])
    .handler(new LegacyChannelInitializer(clientHandshakeHandler))

  def connect(remoteAddress: InetSocketAddress): Unit =
    channels.computeIfAbsent(remoteAddress, _ => {
      val chanF = bootstrap.connect(remoteAddress)
      val chan = chanF.channel()
      allChannels.add(chan)
      log.debug(s"${id(chan)} Connecting to $remoteAddress")
      chan.closeFuture().addListener { (chf: ChannelFuture) =>
        log.debug(s"${id(chf.channel)} Connection to $remoteAddress closed")
        allChannels.remove(chf.channel())
        channels.remove(remoteAddress, chf.channel())
      }
      chan
    })

  def shutdown(): Unit = try {
    log.debug("Unbound server")
    allChannels.close().await()
    log.debug("Closed all channels")
  } finally {
    workerGroup.shutdownGracefully()
    bossGroup.shutdownGracefully()
  }
}
