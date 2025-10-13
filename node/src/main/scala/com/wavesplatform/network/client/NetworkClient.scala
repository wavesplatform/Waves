package com.wavesplatform.network.client

import com.wavesplatform.Version
import com.wavesplatform.network.{Handshake, LegacyFrameCodec, LegacyFrameCodecL1, PeerDatabase, TrafficLogger}
import com.wavesplatform.settings.*
import com.wavesplatform.utils.ScorexLogging
import io.netty.bootstrap.Bootstrap
import io.netty.channel.*
import io.netty.channel.group.{ChannelGroup, DefaultChannelGroup}
import io.netty.channel.nio.NioIoHandler
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.util.concurrent.GlobalEventExecutor

import java.io.IOException
import java.net.InetSocketAddress
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}

class NetworkClient(
    applicationName: String,
    nodeName: String = "network-client",
    nonce: Long = 0L,
    allChannels: ChannelGroup = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE),
    trafficLoggerSettings: TrafficLogger.Settings = TrafficLogger.Settings(Set.empty, Set.empty),
    frameCodec: LegacyFrameCodec = LegacyFrameCodecL1(PeerDatabase.NoOp, 5.seconds)
) extends ScorexLogging {
  def this(trafficLoggerSettings: TrafficLogger.Settings, chainId: Char, nodeName: String, nonce: Long, allChannels: ChannelGroup) =
    this(Constants.ApplicationName + chainId, nodeName, nonce, allChannels, trafficLoggerSettings)

  private val workerGroup = new MultiThreadIoEventLoopGroup(NioIoHandler.newFactory());
  private val handshake   = Handshake(applicationName, Version.VersionTuple, nodeName, nonce, None)

  def connect(remoteAddress: InetSocketAddress): Future[Channel] = {
    val p = Promise[Channel]()

    val bootstrap = new Bootstrap()
      .group(workerGroup)
      .channel(classOf[NioSocketChannel])
      .handler(new LegacyChannelInitializer(frameCodec, trafficLoggerSettings, handshake, p))

    log.debug(s"Connecting to $remoteAddress")
    val connectionFuture = bootstrap.connect(remoteAddress)
    connectionFuture.addListener { (f: ChannelFuture) =>
      log.debug(s"${if (f.isSuccess) "Connected to" else "Could not connect to"} $remoteAddress", f.cause)
      connectionFuture.channel().write(p)
    }

    val channel = connectionFuture.channel()
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
