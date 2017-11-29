package com.wavesplatform.network

import java.net.{InetSocketAddress, NetworkInterface}
import java.nio.channels.ClosedChannelException
import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.metrics.Metrics
import com.wavesplatform.network.MessageObserver.Messages
import com.wavesplatform.settings._
import com.wavesplatform.{UtxPool, Version}
import io.netty.bootstrap.{Bootstrap, ServerBootstrap}
import io.netty.channel._
import io.netty.channel.group.ChannelGroup
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}
import io.netty.util.concurrent.DefaultThreadFactory
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import org.asynchttpclient.netty.channel.NoopHandler
import org.influxdb.dto.Point
import scorex.transaction._
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.duration._

trait NS {
  def connect(remoteAddress: InetSocketAddress): Unit

  def shutdown(): Unit

  val messages: Messages
  val closedChannels: Observable[Channel]
}

object NetworkServer extends ScorexLogging {

  def apply(settings: WavesSettings,
            lastBlockInfos: Observable[LastBlockInfo],
            history: NgHistory,
            utxPool: UtxPool,
            peerDatabase: PeerDatabase,
            allChannels: ChannelGroup,
            peerInfo: ConcurrentHashMap[Channel, PeerInfo]
           ): NS = {
    @volatile var shutdownInitiated = false

    val bossGroup = new NioEventLoopGroup(0, new DefaultThreadFactory("nio-boss-group", true))
    val workerGroup = new NioEventLoopGroup(0, new DefaultThreadFactory("nio-worker-group", true))
    val handshake = Handshake(Constants.ApplicationName + settings.blockchainSettings.addressSchemeCharacter, Version.VersionTuple,
      settings.networkSettings.nodeName, settings.networkSettings.nonce, settings.networkSettings.declaredAddress)

    val trafficWatcher = if (settings.metrics.enable) {
      log.debug("Watching and reporting of traffic is enabled")
      new TrafficWatcher
    } else new NoopHandler

    val trafficLogger = if (settings.networkSettings.trafficLogger.enable) {
      log.debug("Logging of traffic is enabled")
      new TrafficLogger(settings.networkSettings.trafficLogger)
    } else new NoopHandler
    val messageCodec = new MessageCodec(peerDatabase)

    val excludedAddresses: Set[InetSocketAddress] = {
      val localAddresses = if (settings.networkSettings.bindAddress.getAddress.isAnyLocalAddress) {
        NetworkInterface.getNetworkInterfaces.asScala
          .flatMap(_.getInetAddresses.asScala
            .map(a => new InetSocketAddress(a, settings.networkSettings.bindAddress.getPort)))
          .toSet
      } else Set(settings.networkSettings.bindAddress)

      localAddresses ++ settings.networkSettings.declaredAddress.toSet
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

    val historyReplier = new HistoryReplier(history, settings.synchronizationSettings)
    val inboundConnectionFilter: PipelineInitializer.HandlerWrapper = new InboundConnectionFilter(peerDatabase,
      settings.networkSettings.maxInboundConnections,
      settings.networkSettings.maxConnectionsPerHost)

    val closedChannelsSubject = ConcurrentSubject.publish[Channel](Scheduler.singleThread("closed-channels"))
    val (mesageObserver, newtworkMessages) = MessageObserver()
    val discardingHandler = new DiscardingHandler(lastBlockInfos.map(_.ready))
    val peerConnections = new ConcurrentHashMap[PeerKey, Channel](10, 0.9f, 10)
    val serverHandshakeHandler = new HandshakeHandler.Server(handshake, peerInfo, peerConnections, peerDatabase, allChannels)
    val utxPoolSynchronizer = new UtxPoolSynchronizer(utxPool, allChannels)

    def peerSynchronizer: ChannelHandlerAdapter = {
      if (settings.networkSettings.enablePeersExchange) {
        new PeerSynchronizer(peerDatabase, settings.networkSettings.peersBroadcastInterval)
      } else PeerSynchronizer.Disabled
    }

    val serverChannel = settings.networkSettings.declaredAddress.map { _ =>
      new ServerBootstrap()
        .group(bossGroup, workerGroup)
        .channel(classOf[NioServerSocketChannel])
        .childHandler(new PipelineInitializer[SocketChannel](Seq(
          inboundConnectionFilter,
          new HandshakeDecoder(peerDatabase),
          new HandshakeTimeoutHandler(settings.networkSettings.handshakeTimeout),
          serverHandshakeHandler,
          lengthFieldPrepender,
          new LengthFieldBasedFrameDecoder(100 * 1024 * 1024, 0, 4, 0, 4),
          new LegacyFrameCodec(peerDatabase),
          trafficWatcher,
          discardingHandler,
          messageCodec,
          trafficLogger,
          writeErrorHandler,
          peerSynchronizer,
          historyReplier,
          utxPoolSynchronizer,
          mesageObserver,
          fatalErrorHandler)))
        .bind(settings.networkSettings.bindAddress)
        .channel()
    }

    val outgoingChannels = new ConcurrentHashMap[InetSocketAddress, Channel]

    val clientHandshakeHandler = new HandshakeHandler.Client(handshake, peerInfo, peerConnections, peerDatabase, allChannels)

    val bootstrap = new Bootstrap()
      .option(ChannelOption.CONNECT_TIMEOUT_MILLIS, settings.networkSettings.connectionTimeout.toMillis.toInt: Integer)
      .group(workerGroup)
      .channel(classOf[NioSocketChannel])
      .handler(new PipelineInitializer[SocketChannel](Seq(
        new HandshakeDecoder(peerDatabase),
        new HandshakeTimeoutHandler(settings.networkSettings.handshakeTimeout),
        clientHandshakeHandler,
        lengthFieldPrepender,
        new LengthFieldBasedFrameDecoder(100 * 1024 * 1024, 0, 4, 0, 4),
        new LegacyFrameCodec(peerDatabase),
        trafficWatcher,
        discardingHandler,
        messageCodec,
        trafficLogger,
        writeErrorHandler,
        peerSynchronizer,
        historyReplier,
        utxPoolSynchronizer,
        mesageObserver,
        fatalErrorHandler)))

    def formatOutgoingChannelEvent(channel: Channel, event: String) = s"${id(channel)} $event, outgoing channel count: ${outgoingChannels.size()}"

    def handleChannelClosed(remoteAddress: InetSocketAddress)(closeFuture: ChannelFuture): Unit = {
      closedChannelsSubject.onNext(closeFuture.channel())
      outgoingChannels.remove(remoteAddress, closeFuture.channel())
      if (!shutdownInitiated) peerDatabase.suspend(remoteAddress.getAddress)

      if (closeFuture.isSuccess)
        log.trace(formatOutgoingChannelEvent(closeFuture.channel(), "Channel closed (expected)"))
      else
        log.debug(formatOutgoingChannelEvent(closeFuture.channel(), s"Channel closed: ${Option(closeFuture.cause()).map(_.getMessage).getOrElse("no message")}"))

    }

    def handleConnectionAttempt(remoteAddress: InetSocketAddress)(thisConnFuture: ChannelFuture): Unit = {
      if (thisConnFuture.isSuccess) {
        log.trace(formatOutgoingChannelEvent(thisConnFuture.channel(), "Connection established"))
        peerDatabase.touch(remoteAddress)
        thisConnFuture.channel().closeFuture().addListener(handleChannelClosed(remoteAddress))
      } else if (thisConnFuture.cause() != null) {
        peerDatabase.suspend(remoteAddress.getAddress)
        outgoingChannels.remove(remoteAddress, thisConnFuture.channel())
        thisConnFuture.cause() match {
          case e: ClosedChannelException =>
            // this can happen when the node is shut down before connection attempt succeeds
            log.trace(formatOutgoingChannelEvent(
              thisConnFuture.channel(),
              s"Channel closed by connection issue: ${Option(e.getMessage).getOrElse("no message")}"
            ))
          case other => log.debug(formatOutgoingChannelEvent(thisConnFuture.channel(), other.getMessage))
        }
      }
    }


    def doConnect(remoteAddress: InetSocketAddress): Unit =
      outgoingChannels.computeIfAbsent(remoteAddress, _ => {
        val newConnFuture = bootstrap.connect(remoteAddress)

        log.trace(s"${id(newConnFuture.channel())} Connecting to $remoteAddress")
        newConnFuture.addListener(handleConnectionAttempt(remoteAddress)).channel()
      })

    val connectTask = workerGroup.scheduleWithFixedDelay(1.second, 5.seconds) {
      import scala.collection.JavaConverters._
      val outgoing = outgoingChannels.keySet.iterator().asScala.toVector

      def outgoingStr = outgoing.map(_.toString).sorted.mkString("[", ", ", "]")

      val all = peerInfo.values().iterator().asScala.flatMap(_.declaredAddress).toVector
      val incoming = all.filterNot(outgoing.contains)

      def incomingStr = incoming.map(_.toString).sorted.mkString("[", ", ", "]")

      log.trace(s"Outgoing: $outgoingStr ++ incoming: $incomingStr")
      if (outgoingChannels.size() < settings.networkSettings.maxOutboundConnections) {
        peerDatabase
          .randomPeer(excluded = excludedAddresses ++ all)
          .foreach(doConnect)
      }

      Metrics.write(
        Point
          .measurement("connections")
          .addField("outgoing", outgoingStr)
          .addField("incoming", incomingStr)
          .addField("n", all.size)
      )
    }




    def doShutdown(): Unit = try {
      shutdownInitiated = true
      connectTask.cancel(false)
      serverChannel.foreach(_.close().await())
      log.debug("Unbound server")
      allChannels.close().await()
      log.debug("Closed all channels")
    } finally {
      workerGroup.shutdownGracefully().await()
      bossGroup.shutdownGracefully().await()
    }

    new NS {
      override def connect(remoteAddress: InetSocketAddress): Unit = doConnect(remoteAddress)

      override def shutdown(): Unit = doShutdown()

      override val messages: Messages = newtworkMessages

      override val closedChannels: Observable[Channel] = closedChannelsSubject
    }
  }
}