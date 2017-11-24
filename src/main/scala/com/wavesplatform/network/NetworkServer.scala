package com.wavesplatform.network

import java.net.{InetSocketAddress, NetworkInterface}
import java.nio.channels.ClosedChannelException
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import com.google.common.cache.CacheBuilder
import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.metrics.Metrics
import com.wavesplatform.mining.Miner
import com.wavesplatform.settings._
import com.wavesplatform.state2._
import com.wavesplatform.{UtxPool, Version}
import io.netty.bootstrap.{Bootstrap, ServerBootstrap}
import io.netty.channel._
import io.netty.channel.group.ChannelGroup
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}
import io.netty.util.concurrent.DefaultThreadFactory
import org.asynchttpclient.netty.channel.NoopHandler
import org.influxdb.dto.Point
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.collection.JavaConverters._
import scala.concurrent.duration._

class NetworkServer(checkpointService: CheckpointService,
                    blockchainUpdater: BlockchainUpdater,
                    time: Time,
                    miner: Miner,
                    stateReader: StateReader,
                    settings: WavesSettings,
                    history: NgHistory,
                    utxPool: UtxPool,
                    peerDatabase: PeerDatabase,
                    allChannels: ChannelGroup,
                    peerInfo: ConcurrentHashMap[Channel, PeerInfo],
                    blockchainReadiness: AtomicBoolean,
                    featureProvider: FeatureProvider) extends ScorexLogging {

  @volatile
  private var shutdownInitiated = false

  private val bossGroup = new NioEventLoopGroup(0, new DefaultThreadFactory("nio-boss-group", true))
  private val workerGroup = new NioEventLoopGroup(0, new DefaultThreadFactory("nio-worker-group", true))
  private val handshake =
    Handshake(Constants.ApplicationName + settings.blockchainSettings.addressSchemeCharacter, Version.VersionTuple,
      settings.networkSettings.nodeName, settings.networkSettings.nonce, settings.networkSettings.declaredAddress)

  private val scoreObserver = new RemoteScoreObserver(
    settings.synchronizationSettings.scoreTTL,
    history.lastBlockIds(settings.synchronizationSettings.maxRollback), history.score())

  private val trafficWatcher = if (settings.metrics.enable) new TrafficWatcher else new NoopHandler
  private val discardingHandler = new DiscardingHandler(blockchainReadiness)
  private val messageCodec = new MessageCodec(peerDatabase)

  private val excludedAddresses: Set[InetSocketAddress] = {
    val localAddresses = if (settings.networkSettings.bindAddress.getAddress.isAnyLocalAddress) {
      NetworkInterface.getNetworkInterfaces.asScala
        .flatMap(_.getInetAddresses.asScala
          .map(a => new InetSocketAddress(a, settings.networkSettings.bindAddress.getPort)))
        .toSet
    } else Set(settings.networkSettings.bindAddress)

    localAddresses ++ settings.networkSettings.declaredAddress.toSet
  }

  private val lengthFieldPrepender = new LengthFieldPrepender(4)

  // There are two error handlers by design. WriteErrorHandler adds a future listener to make sure writes to network
  // succeed. It is added to the head of pipeline (it's the closest of the two to actual network), because some writes
  // are initiated from the middle of the pipeline (e.g. extension requests). FatalErrorHandler, on the other hand,
  // reacts to inbound exceptions (the ones thrown during channelRead). It is added to the tail of pipeline to handle
  // exceptions bubbling up from all the handlers below. When a fatal exception is caught (like OutOfMemory), the
  // application is terminated.
  private val writeErrorHandler = new WriteErrorHandler
  private val fatalErrorHandler = new FatalErrorHandler
  private val historyReplier = new HistoryReplier(history, settings.synchronizationSettings)
  private val inboundConnectionFilter: PipelineInitializer.HandlerWrapper = new InboundConnectionFilter(peerDatabase,
    settings.networkSettings.maxInboundConnections,
    settings.networkSettings.maxConnectionsPerHost)

  private val knownInvalidBlocks = {
    val residenceTime = 1.day

    CacheBuilder
      .newBuilder()
      .expireAfterWrite(residenceTime.length, residenceTime.unit)
      .build[ByteStr, Object]()
  }

  private def isInvalidBlock(id: ByteStr): Boolean = knownInvalidBlocks.getIfPresent(id) != null

  private val microBlockOwners = new MicroBlockOwners(settings.synchronizationSettings.microBlockSynchronizer.invCacheTimeout)
  private val coordinatorHandler = {
    val dummy = new Object()
    new CoordinatorHandler(
      checkpointService, history, blockchainUpdater, time, stateReader, utxPool, blockchainReadiness, miner, settings,
      peerDatabase, allChannels, featureProvider, microBlockOwners, knownInvalidBlocks.put(_, dummy)
    )
  }

  private val peerConnections = new ConcurrentHashMap[PeerKey, Channel](10, 0.9f, 10)

  private val serverHandshakeHandler =
    new HandshakeHandler.Server(handshake, peerInfo, peerConnections, peerDatabase, allChannels)

  private val utxPoolSynchronizer = new UtxPoolSynchronizer(utxPool, allChannels)
  private val microBlockSynchronizer = new MicroBlockSynchronizer(
    settings.synchronizationSettings.microBlockSynchronizer,
    history,
    peerDatabase,
    blockchainUpdater.lastBlockId,
    microBlockOwners
  )


  private val serverChannel = settings.networkSettings.declaredAddress.map { _ =>
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
        writeErrorHandler,
        peerSynchronizer,
        historyReplier,
        microBlockSynchronizer,
        new ExtensionSignaturesLoader(settings.synchronizationSettings.synchronizationTimeout, peerDatabase, isInvalidBlock),
        new ExtensionBlocksLoader(settings.synchronizationSettings.synchronizationTimeout, peerDatabase, history),
        new OptimisticExtensionLoader,
        utxPoolSynchronizer,
        scoreObserver,
        coordinatorHandler,
        fatalErrorHandler)))
      .bind(settings.networkSettings.bindAddress)
      .channel()
  }

  private val outgoingChannels = new ConcurrentHashMap[InetSocketAddress, Channel]

  private val clientHandshakeHandler =
    new HandshakeHandler.Client(handshake, peerInfo, peerConnections, peerDatabase, allChannels)

  private val bootstrap = new Bootstrap()
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
      writeErrorHandler,
      peerSynchronizer,
      historyReplier,
      microBlockSynchronizer,
      new ExtensionSignaturesLoader(settings.synchronizationSettings.synchronizationTimeout, peerDatabase, isInvalidBlock),
      new ExtensionBlocksLoader(settings.synchronizationSettings.synchronizationTimeout, peerDatabase, history),
      new OptimisticExtensionLoader,
      utxPoolSynchronizer,
      scoreObserver,
      coordinatorHandler,
      fatalErrorHandler)))

  private val connectTask = workerGroup.scheduleWithFixedDelay(1.second, 5.seconds) {
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
        .foreach(connect)
    }

    Metrics.write(
      Point
        .measurement("connections")
        .addField("outgoing", outgoingStr)
        .addField("incoming", incomingStr)
        .addField("n", all.size)
    )
  }

  private def peerSynchronizer: ChannelHandlerAdapter = {
    if (settings.networkSettings.enablePeersExchange) {
      new PeerSynchronizer(peerDatabase, settings.networkSettings.peersBroadcastInterval)
    } else PeerSynchronizer.Disabled
  }

  private def handleChannelClosed(remoteAddress: InetSocketAddress)(closeFuture: ChannelFuture): Unit = {
    outgoingChannels.remove(remoteAddress, closeFuture.channel())
    if (!shutdownInitiated) peerDatabase.suspend(remoteAddress.getAddress)

    if (closeFuture.isSuccess) {
      log.trace(formatOutgoingChannelEvent(closeFuture.channel(), "Channel closed (expected)"))
    } else {
      log.debug(formatOutgoingChannelEvent(
        closeFuture.channel(),
        s"Channel closed: ${Option(closeFuture.cause()).map(_.getMessage).getOrElse("no message")}"
      ))
    }
  }

  private def formatOutgoingChannelEvent(channel: Channel, event: String) =
    s"${id(channel)} $event, outgoing channel count: ${outgoingChannels.size()}"

  private def handleConnectionAttempt(remoteAddress: InetSocketAddress)(thisConnFuture: ChannelFuture): Unit = {
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

  def connect(remoteAddress: InetSocketAddress): Unit =
    outgoingChannels.computeIfAbsent(remoteAddress, _ => {
      val newConnFuture = bootstrap.connect(remoteAddress)

      log.trace(s"${id(newConnFuture.channel())} Connecting to $remoteAddress")
      newConnFuture.addListener(handleConnectionAttempt(remoteAddress)).channel()
    })

  def shutdown(): Unit = try {
    shutdownInitiated = true
    connectTask.cancel(false)
    serverChannel.foreach(_.close().await())
    log.debug("Unbound server")
    allChannels.close().await()
    log.debug("Closed all channels")
    coordinatorHandler.shutdown()
  } finally {
    workerGroup.shutdownGracefully().await()
    bossGroup.shutdownGracefully().await()
  }
}
