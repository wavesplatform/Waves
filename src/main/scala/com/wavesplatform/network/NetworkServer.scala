package com.wavesplatform.network

import java.net.{InetSocketAddress, NetworkInterface}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import com.wavesplatform.mining.Miner
import com.wavesplatform.settings._
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.{UtxPool, Version}
import io.netty.bootstrap.{Bootstrap, ServerBootstrap}
import io.netty.channel._
import io.netty.channel.group.ChannelGroup
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}
import scorex.network.message.MessageSpec
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
                    history: History,
                    utxPool: UtxPool,
                    peerDatabase: PeerDatabase,
                    allChannels: ChannelGroup,
                    peerInfo: ConcurrentHashMap[Channel, PeerInfo],
                    blockchainReadiness: AtomicBoolean
                   ) extends ScorexLogging {

  @volatile
  private var shutdownInitiated = false

  private val bossGroup = new NioEventLoopGroup()
  private val workerGroup = new NioEventLoopGroup()
  private val handshake =
    Handshake(Constants.ApplicationName + settings.blockchainSettings.addressSchemeCharacter, Version.VersionTuple,
      settings.networkSettings.nodeName, settings.networkSettings.nonce, settings.networkSettings.declaredAddress)

  private val scoreObserver = new RemoteScoreObserver(
    settings.synchronizationSettings.scoreTTL,
    history.lastBlockIds(settings.synchronizationSettings.maxRollback), history.score())

  private val discardingHandler = new DiscardingHandler(blockchainReadiness)
  private val messageCodec = new MessageCodec()

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

  private val peerSynchronizer = new PeerSynchronizer(peerDatabase, settings.networkSettings.peersBroadcastInterval)
  // There are two error handlers by design. WriteErrorHandler adds a future listener to make sure writes to network
  // succeed. It is added to the head of pipeline (it's the closest of the two to actual network), because some writes
  // are initiated from the middle of the pipeline (e.g. extension requests). FatalErrorHandler, on the other hand,
  // reacts to inbound exceptions (the ones thrown during channelRead). It is added to the tail of pipeline to handle
  // exceptions bubbling up from all the handlers below. When a fatal exception is caught (like OutOfMemory), the
  // application is terminated.
  private val writeErrorHandler = new WriteErrorHandler
  private val fatalErrorHandler = new FatalErrorHandler
  private val historyReplier = new HistoryReplier(history, settings.synchronizationSettings.maxChainLength)
  private val inboundConnectionFilter: PipelineInitializer.HandlerWrapper = new InboundConnectionFilter(peerDatabase,
    settings.networkSettings.maxInboundConnections,
    settings.networkSettings.maxConnectionsPerHost)

  private val coordinatorExecutor = new DefaultEventLoop

  private val coordinatorHandler = new CoordinatorHandler(checkpointService, history, blockchainUpdater, time,
    stateReader, utxPool, blockchainReadiness, miner, settings, peerDatabase, allChannels)
  private val peerUniqueness = new ConcurrentHashMap[PeerKey, Channel]()

  private val serverHandshakeHandler =
    new HandshakeHandler.Server(handshake, peerInfo, peerUniqueness, peerDatabase, allChannels)

  private val utxPoolSynchronizer = new UtxPoolSynchronizer(utxPool)

  private val serverChannel = settings.networkSettings.declaredAddress.map { _ =>
    new ServerBootstrap()
      .group(bossGroup, workerGroup)
      .channel(classOf[NioServerSocketChannel])
      .childHandler(new PipelineInitializer[SocketChannel](Seq(
        inboundConnectionFilter,
        writeErrorHandler,
        new HandshakeDecoder,
        new HandshakeTimeoutHandler(settings.networkSettings.handshakeTimeout),
        serverHandshakeHandler,
        lengthFieldPrepender,
        new LengthFieldBasedFrameDecoder(1024 * 1024, 0, 4, 0, 4),
        new LegacyFrameCodec,
        discardingHandler,
        messageCodec,
        peerSynchronizer,
        historyReplier,
        new ExtensionSignaturesLoader(settings.synchronizationSettings.synchronizationTimeout, peerDatabase),
        new ExtensionBlocksLoader(history, settings.synchronizationSettings.synchronizationTimeout, peerDatabase),
        new OptimisticExtensionLoader,
        utxPoolSynchronizer,
        scoreObserver,
        coordinatorHandler -> coordinatorExecutor,
        fatalErrorHandler)))
      .bind(settings.networkSettings.bindAddress)
      .channel()
  }

  private val outgoingChannelCount = new AtomicInteger(0)

  private val outgoingChannels = new ConcurrentHashMap[InetSocketAddress, Channel]

  private def incomingDeclaredAddresses =
    peerInfo.values().asScala.flatMap(_.declaredAddress)

  private val clientHandshakeHandler =
    new HandshakeHandler.Client(handshake, peerInfo, peerUniqueness, peerDatabase)


  private val bootstrap = new Bootstrap()
    .option(ChannelOption.CONNECT_TIMEOUT_MILLIS, settings.networkSettings.connectionTimeout.toMillis.toInt: Integer)
    .group(workerGroup)
    .channel(classOf[NioSocketChannel])
    .handler(new PipelineInitializer[SocketChannel](Seq(
      writeErrorHandler,
      new HandshakeDecoder,
      new HandshakeTimeoutHandler(settings.networkSettings.handshakeTimeout),
      clientHandshakeHandler,
      lengthFieldPrepender,
      new LengthFieldBasedFrameDecoder(1024 * 1024, 0, 4, 0, 4),
      new LegacyFrameCodec,
      discardingHandler,
      messageCodec,
      peerSynchronizer,
      historyReplier,
      new ExtensionSignaturesLoader(settings.synchronizationSettings.synchronizationTimeout, peerDatabase),
      new ExtensionBlocksLoader(history, settings.synchronizationSettings.synchronizationTimeout, peerDatabase),
      new OptimisticExtensionLoader,
      utxPoolSynchronizer,
      scoreObserver,
      coordinatorHandler -> coordinatorExecutor,
      fatalErrorHandler)))

  private val connectTask = workerGroup.scheduleWithFixedDelay(1.second, 5.seconds) {
    log.trace(s"Outgoing: ${outgoingChannels.keySet} ++ incoming: $incomingDeclaredAddresses")
    if (outgoingChannelCount.get() < settings.networkSettings.maxOutboundConnections) {
      peerDatabase
        .randomPeer(excludedAddresses ++ outgoingChannels.keySet().asScala ++ incomingDeclaredAddresses)
        .foreach(connect)
    }
  }

  def connect(remoteAddress: InetSocketAddress): Unit =
    outgoingChannels.computeIfAbsent(remoteAddress, _ => {
      log.debug(s"Connecting to $remoteAddress")
      bootstrap.connect(remoteAddress)
        .addListener { (connFuture: ChannelFuture) =>
          if (connFuture.isDone) {
            if (connFuture.cause() != null) {
              log.debug(s"${id(connFuture.channel())} Connection failed, blacklisting $remoteAddress", connFuture.cause())
              peerDatabase.blacklist(remoteAddress.getAddress)
            } else if (connFuture.isSuccess) {
              log.debug(s"${id(connFuture.channel())} Connection established")
              peerDatabase.touch(remoteAddress)
              outgoingChannelCount.incrementAndGet()
              connFuture.channel().closeFuture().addListener { (closeFuture: ChannelFuture) =>
                val remainingCount = outgoingChannelCount.decrementAndGet()
                log.debug(s"${id(closeFuture.channel)} Connection closed, $remainingCount outgoing channel(s) remaining")
                allChannels.remove(closeFuture.channel())
                outgoingChannels.remove(remoteAddress, closeFuture.channel())
                if (!shutdownInitiated) peerDatabase.blacklist(remoteAddress.getAddress)
              }
              allChannels.add(connFuture.channel())
            }
          }
        }.channel()
    })


  def shutdown(): Unit = try {
    shutdownInitiated = true
    connectTask.cancel(false)
    serverChannel.foreach(_.close().await())
    log.debug("Unbound server")
    allChannels.close().await()
    log.debug("Closed all channels")
  } finally {
    workerGroup.shutdownGracefully().await()
    bossGroup.shutdownGracefully().await()
    coordinatorExecutor.shutdownGracefully().await()
  }
}
