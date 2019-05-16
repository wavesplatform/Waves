package com.wavesplatform.network.client

import java.io.IOException
import java.net.InetSocketAddress

import com.wavesplatform.Version
import com.wavesplatform.network.{Handshake, TrafficLogger}
import com.wavesplatform.settings._
import com.wavesplatform.utils.ScorexLogging
import io.netty.bootstrap.Bootstrap
import io.netty.channel._
import io.netty.channel.group.ChannelGroup
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel

import scala.concurrent.{Future, Promise}

class NetworkClient(trafficLoggerSettings: TrafficLogger.Settings, chainId: Char, nodeName: String, nonce: Long, allChannels: ChannelGroup) extends ScorexLogging {

  private val workerGroup = new NioEventLoopGroup()
  private val handshake   = Handshake(Constants.ApplicationName + chainId, Version.VersionTuple, nodeName, nonce, None)

  def connect(remoteAddress: InetSocketAddress): Future[Channel] = {
    val p = Promise[Channel]

    val bootstrap = new Bootstrap()
      .group(workerGroup)
      .channel(classOf[NioSocketChannel])
      .handler(new LegacyChannelInitializer(trafficLoggerSettings, handshake, p))

    log.debug(s"Connecting to $remoteAddress")
    val channelFuture = bootstrap.connect(remoteAddress)
    channelFuture.addListener((_: io.netty.util.concurrent.Future[Void]) => {
      log.debug(s"Connected to $remoteAddress")
      channelFuture.channel().write(p)
    })

    val channel = channelFuture.channel()
    allChannels.add(channel)
    channel.closeFuture().addListener { (chf: ChannelFuture) =>
      if (!p.isCompleted) {
        val cause = Option(chf.cause()).getOrElse(new IllegalStateException("The connection is closed before handshake"))
        p.failure(new IOException(cause))
      }
      log.debug(s"Connection to $remoteAddress closed")
      allChannels.remove(chf.channel())
    }

    p.future
  }

  def shutdown(): Unit =
    try {
      allChannels.close().await()
      log.debug("Closed all channels")
    } finally {
      workerGroup.shutdownGracefully()
    }
}
