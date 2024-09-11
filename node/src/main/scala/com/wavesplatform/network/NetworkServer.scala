package com.wavesplatform.network

import com.wavesplatform.Version
import com.wavesplatform.metrics.Metrics
import com.wavesplatform.settings.*
import com.wavesplatform.state.Cast
import com.wavesplatform.utils.ScorexLogging
import io.netty.bootstrap.{Bootstrap, ServerBootstrap}
import io.netty.channel.*
import io.netty.channel.group.ChannelGroup
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}
import io.netty.util.concurrent.DefaultThreadFactory
import monix.reactive.Observable
import org.influxdb.dto.Point

import java.net.{InetSocketAddress, NetworkInterface, SocketAddress}
import java.nio.channels.ClosedChannelException
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Random

trait NetworkServer {
  def connect(remoteAddress: InetSocketAddress): Unit
  def shutdown(): Unit
  def closedChannels: Observable[Channel]
}

object NetworkServer extends ScorexLogging {
  val MaxFrameLength: Int                  = 100 * 1024 * 1024
  private[this] val AverageHandshakePeriod = 1.second
  private[this] val LengthFieldSize        = 4

  def apply(
      applicationName: String,
      networkSettings: NetworkSettings,
      peerDatabase: PeerDatabase,
      allChannels: ChannelGroup,
      peerInfo: ConcurrentHashMap[Channel, PeerInfo],
      protocolSpecificPipeline: => Seq[ChannelHandlerAdapter],
  ): NetworkServer = {
    @volatile var shutdownInitiated = false

    val bossGroup   = new NioEventLoopGroup(0, new DefaultThreadFactory("nio-boss-group", true))
    val workerGroup = new NioEventLoopGroup(0, new DefaultThreadFactory("nio-worker-group", true))
    val handshake = Handshake(
      applicationName,
      Version.VersionTuple,
      networkSettings.nodeName,
      networkSettings.nonce,
      networkSettings.declaredAddress
    )

    val excludedAddresses: Set[InetSocketAddress] =
      networkSettings.bindAddress.fold(Set.empty[InetSocketAddress]) { bindAddress =>
        val isLocal = Option(bindAddress.getAddress).exists(_.isAnyLocalAddress)
        val localAddresses = if (isLocal) {
          NetworkInterface.getNetworkInterfaces.asScala
            .flatMap(_.getInetAddresses.asScala.map(a => new InetSocketAddress(a, bindAddress.getPort)))
            .toSet
        } else Set(bindAddress)

        localAddresses ++ networkSettings.declaredAddress.toSet
      }

    val lengthFieldPrepender = new LengthFieldPrepender(4)

    // There are two error handlers by design. WriteErrorHandler adds a future listener to make sure writes to network
    // succeed. It is added to the head of pipeline (it's the closest of the two to actual network), because some writes
    // are initiated from the middle of the pipeline (e.g. extension requests). FatalErrorHandler, on the other hand,
    // reacts to inbound exceptions (the ones thrown during channelRead). It is added to the tail of pipeline to handle
    // exceptions bubbling up from all the handlers below. When a fatal exception is caught (like OutOfMemory), the
    // application is terminated.
    val writeErrorHandler = new WriteErrorHandler
    val fatalErrorHandler = new FatalErrorHandler

    val inboundConnectionFilter =
      new InboundConnectionFilter(peerDatabase, networkSettings.maxInboundConnections, networkSettings.maxConnectionsPerHost)

    val (channelClosedHandler, closedChannelsSubject) = ChannelClosedHandler()
    val peerConnectionsMap                            = new ConcurrentHashMap[PeerKey, Channel](10, 0.9f, 10)
    val serverHandshakeHandler = new HandshakeHandler.Server(handshake, peerInfo, peerConnectionsMap, peerDatabase, allChannels)

    def pipelineTail: Seq[ChannelHandlerAdapter] =
      Seq(
        lengthFieldPrepender,
        new LengthFieldBasedFrameDecoder(MaxFrameLength, 0, LengthFieldSize, 0, LengthFieldSize)
      ) ++ protocolSpecificPipeline ++
        Seq(writeErrorHandler, channelClosedHandler, fatalErrorHandler)

    val serverChannel = networkSettings.bindAddress.map { bindAddress =>
      new ServerBootstrap()
        .group(bossGroup, workerGroup)
        .channel(classOf[NioServerSocketChannel])
        .childHandler(
          new PipelineInitializer(
            Seq(
              inboundConnectionFilter,
              new BrokenConnectionDetector(networkSettings.breakIdleConnectionsTimeout),
              new HandshakeDecoder(peerDatabase),
              new HandshakeTimeoutHandler(networkSettings.handshakeTimeout),
              serverHandshakeHandler
            ) ++ pipelineTail
          )
        )
        .bind(bindAddress)
        .channel()
    }

    val outgoingChannels = new ConcurrentHashMap[InetSocketAddress, Channel]

    val clientHandshakeHandler = new HandshakeHandler.Client(handshake, peerInfo, peerConnectionsMap, peerDatabase, allChannels)

    val bootstrap = new Bootstrap()
      .option(ChannelOption.CONNECT_TIMEOUT_MILLIS, networkSettings.connectionTimeout.toMillis.toInt: Integer)
      .group(workerGroup)
      .channel(classOf[NioSocketChannel])
      .handler(
        new PipelineInitializer(
          Seq(
            new BrokenConnectionDetector(networkSettings.breakIdleConnectionsTimeout),
            new HandshakeDecoder(peerDatabase),
            new HandshakeTimeoutHandler(if (peerConnectionsMap.isEmpty) AverageHandshakePeriod else networkSettings.handshakeTimeout),
            clientHandshakeHandler
          ) ++ pipelineTail
        )
      )

    def formatOutgoingChannelEvent(channel: Channel, event: String) = s"${id(channel)} $event, outgoing channel count: ${outgoingChannels.size()}"

    def handleOutgoingChannelClosed(remoteAddress: InetSocketAddress)(closeFuture: ChannelFuture): Unit = {
      outgoingChannels.remove(remoteAddress, closeFuture.channel())
      if (!shutdownInitiated) peerDatabase.suspendAndClose(closeFuture.channel())

      if (closeFuture.isSuccess)
        log.trace(formatOutgoingChannelEvent(closeFuture.channel(), "Channel closed (expected)"))
      else
        log.debug(
          formatOutgoingChannelEvent(
            closeFuture.channel(),
            s"Channel closed: ${Option(closeFuture.cause()).map(_.getMessage).getOrElse("no message")}"
          )
        )

      logConnections()
    }

    def handleConnectionAttempt(remoteAddress: InetSocketAddress)(thisConnFuture: ChannelFuture): Unit = {
      if (thisConnFuture.isSuccess) {
        thisConnFuture.channel().closeFuture().addListener(f => handleOutgoingChannelClosed(remoteAddress)(f))
      } else if (thisConnFuture.cause() != null) {
        peerDatabase.suspend(remoteAddress)
        outgoingChannels.remove(remoteAddress, thisConnFuture.channel())
        thisConnFuture.cause() match {
          case e: ClosedChannelException =>
            // this can happen when the node is shut down before connection attempt succeeds
            log.trace(
              formatOutgoingChannelEvent(
                thisConnFuture.channel(),
                s"Channel closed by connection issue: ${Option(e.getMessage).getOrElse("no message")}"
              )
            )
          case other => log.debug(formatOutgoingChannelEvent(thisConnFuture.channel(), other.getMessage))
        }
      }
      logConnections()
    }

    def doConnect(remoteAddress: InetSocketAddress): Unit =
      outgoingChannels.computeIfAbsent(
        remoteAddress,
        _ => {
          val newConnFuture = bootstrap.connect(remoteAddress)

          log.trace(s"${id(newConnFuture.channel())} Connecting to $remoteAddress")
          newConnFuture.addListener(f => handleConnectionAttempt(remoteAddress)(f)).channel()
        }
      )

    def logConnections(): Unit = {
      def mkAddressString(addresses: IterableOnce[SocketAddress]) =
        addresses.iterator.map(_.toString).toVector.sorted.mkString("[", ",", "]")

      val incoming = peerInfo.values().asScala.view.map(_.remoteAddress).filterNot(outgoingChannels.containsKey)

      lazy val incomingStr = mkAddressString(incoming)
      lazy val outgoingStr = mkAddressString(outgoingChannels.keySet.iterator().asScala)

      val all = peerInfo.values().iterator().asScala.flatMap(_.remoteAddress.cast[InetSocketAddress])

      log.trace(s"Outgoing: $outgoingStr ++ incoming: $incomingStr")

      Metrics.write(
        Point
          .measurement("connections")
          .addField("outgoing", outgoingStr)
          .addField("incoming", incomingStr)
          .addField("n", all.size)
      )
    }

    def scheduleConnectTask(): Unit = if (!shutdownInitiated) {
      val delay = (if (peerConnectionsMap.isEmpty) AverageHandshakePeriod else 5.seconds) +
        (Random.nextInt(1000) - 500).millis // add some noise so that nodes don't attempt to connect to each other simultaneously

      workerGroup.schedule(delay) {
        if (outgoingChannels.size() < networkSettings.maxOutboundConnections) {
          val all = peerInfo.values().iterator().asScala.flatMap(_.remoteAddress.cast[InetSocketAddress])
          peerDatabase
            .randomPeer(excluded = excludedAddresses ++ all)
            .foreach(doConnect)
        }

        scheduleConnectTask()
      }
    }

    scheduleConnectTask()

    new NetworkServer {
      override def connect(remoteAddress: InetSocketAddress): Unit = doConnect(remoteAddress)

      override def shutdown(): Unit =
        try {
          shutdownInitiated = true
          serverChannel.foreach(_.close().await())
          log.debug("Unbound server")
          allChannels.close().await()
          log.debug("Closed all channels")
        } finally {
          workerGroup.shutdownGracefully().await()
          bossGroup.shutdownGracefully().await()
          channelClosedHandler.shutdown()
        }

      override val closedChannels: Observable[Channel] = closedChannelsSubject
    }
  }
}
