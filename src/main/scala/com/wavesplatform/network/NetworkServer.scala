package com.wavesplatform.network

import java.net.{InetSocketAddress, NetworkInterface}
import java.util.Collections
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.util.stream.Collectors

import com.wavesplatform.mining.Miner
import com.wavesplatform.settings._
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.{Coordinator, Version}
import io.netty.bootstrap.{Bootstrap, ServerBootstrap}
import io.netty.channel._
import io.netty.channel.group.ChannelGroup
import io.netty.channel.local.{LocalAddress, LocalChannel, LocalServerChannel}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}
import scorex.network.message.{BasicMessagesRepo, MessageSpec}
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}
import scorex.wallet.Wallet

import scala.collection.JavaConverters._
import scala.concurrent.duration._

class NetworkServer(
    chainId: Char,
    bindAddress: InetSocketAddress,
    declaredAddress: Option[InetSocketAddress],
    settings: WavesSettings,
    history: History,
    checkpoints: CheckpointService,
    blockchainUpdater: BlockchainUpdater,
    time: Time,
    stateReader: StateReader,
    utxStorage: UnconfirmedTransactionsStorage,
    txHandler: NewTransactionHandler,
    peerDatabase: PeerDatabase,
    wallet: Wallet,
    allChannels: ChannelGroup,
    peerInfo: ConcurrentHashMap[Channel, PeerInfo]) extends ScorexLogging {

  private val bossGroup = new NioEventLoopGroup()
  private val workerGroup = new NioEventLoopGroup()
  private val handshake =
    Handshake(Constants.ApplicationName + chainId, Version.VersionTuple, settings.networkSettings.nodeName,
      settings.networkSettings.nonce, settings.networkSettings.declaredAddress)

  private val scoreObserver = new RemoteScoreObserver(
      settings.synchronizationSettings.scoreTTL,
      history.lastBlockIds(settings.synchronizationSettings.maxRollback))

  private val blockchainReadiness = new AtomicBoolean(false)
  def setBlockchainExpired(expired: Boolean): Unit = blockchainReadiness.compareAndSet(expired, !expired)

  private val discardingHandler = new DiscardingHandler(blockchainReadiness.get())
  private val specs: Map[Byte, MessageSpec[_ <: AnyRef]] = (BasicMessagesRepo.specs ++ TransactionalMessagesRepo.specs).map(s => s.messageCode -> s).toMap
  private val messageCodec = new MessageCodec(specs)

  private val excludedAddresses: Set[InetSocketAddress] = {
    val localAddresses = if (bindAddress.getAddress.isAnyLocalAddress) {
      import Collections.list
      list(NetworkInterface.getNetworkInterfaces)
        .stream()
        .flatMap[InetSocketAddress] { i =>
          list(i.getInetAddresses)
            .stream()
            .map(a => new InetSocketAddress(a, settings.networkSettings.bindAddress.getPort))
        }
        .collect(Collectors.toSet())
        .asScala.toSet
    } else Set(settings.networkSettings.bindAddress)

    localAddresses ++ settings.networkSettings.declaredAddress.toSet
  }

  private val lengthFieldPrepender = new LengthFieldPrepender(4)

  private val miner = new Miner(history, stateReader, utxStorage, wallet.privateKeyAccounts(),
    settings.blockchainSettings, settings.minerSettings, time.correctedTime(), allChannels.size(),
    b => writeToLocalChannel(BlockForged(b)))

  private val peerSynchronizer = new PeerSynchronizer(peerDatabase)
  private val utxPoolSynchronizer = new UtxPoolSynchronizer(txHandler, allChannels)
  private val errorHandler = new ErrorHandler(peerDatabase)
  private val historyReplier = new HistoryReplier(history, settings.synchronizationSettings.maxChainLength)

  private val inboundConnectionFilter = new InboundConnectionFilter(peerDatabase,
    settings.networkSettings.maxInboundConnections,
    settings.networkSettings.maxConnectionsPerHost)

  private val coordinatorExecutor = new DefaultEventLoop
  private val coordinator = new Coordinator(checkpoints, history, blockchainUpdater, stateReader, utxStorage,
    time.correctedTime(), settings.blockchainSettings,
    settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed, settings.checkpointsSettings.publicKey,
    miner, setBlockchainExpired)

  private val coordinatorHandler = new CoordinatorHandler(coordinator, peerDatabase, allChannels)

  private val address = new LocalAddress("local-events-channel")
  private val localServerGroup = new DefaultEventLoopGroup()
  private val localServer = new ServerBootstrap()
    .group(localServerGroup)
    .channel(classOf[LocalServerChannel])
    .childHandler(new PipelineInitializer[LocalChannel](Seq(
      utxPoolSynchronizer, scoreObserver,
      coordinatorHandler -> coordinatorExecutor)
    ))

  localServer.bind(address).sync()

  private val localClientGroup = new DefaultEventLoopGroup()
  val localClientChannel = new Bootstrap()
    .group(localClientGroup)
    .channel(classOf[LocalChannel])
    .handler(new PipelineInitializer[LocalChannel](Seq.empty))
    .connect(address)
    .channel()

  log.info(s"${id(localClientChannel)} Local channel opened")

  private val peerUniqueness = new ConcurrentHashMap[PeerKey, Channel]()

  private val serverHandshakeHandler =
    new HandshakeHandler.Server(handshake, peerInfo, peerUniqueness, peerDatabase, allChannels)

  private val serverChannel = declaredAddress.map { _ =>
    new ServerBootstrap()
      .group(bossGroup, workerGroup)
      .channel(classOf[NioServerSocketChannel])
      .childHandler(new PipelineInitializer[SocketChannel](Seq(
        inboundConnectionFilter,
        errorHandler,
        new HandshakeDecoder,
        new HandshakeTimeoutHandler,
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
        coordinatorHandler -> coordinatorExecutor)))
      .bind(bindAddress)
      .channel()
  }

  private val outgoingChannelCount = new AtomicInteger(0)
  private val outgoingChannels = new ConcurrentHashMap[InetSocketAddress, Channel]

  private def incomingDeclaredAddresses =
    peerInfo.values().asScala.flatMap(_.declaredAddress)

  private val clientHandshakeHandler =
    new HandshakeHandler.Client(handshake, peerInfo, peerUniqueness, peerDatabase)

  private val bootstrap = new Bootstrap()
    .group(workerGroup)
    .channel(classOf[NioSocketChannel])
    .handler(new PipelineInitializer[SocketChannel](Seq(
      errorHandler,
      new HandshakeDecoder,
      new HandshakeTimeoutHandler,
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
      coordinatorHandler -> coordinatorExecutor)))

  val connectTask = workerGroup.scheduleWithFixedDelay(1.second, 5.seconds) {
    if (outgoingChannelCount.get() < settings.networkSettings.maxOutboundConnections) {
      peerDatabase
        .getRandomPeer(excludedAddresses ++ outgoingChannels.keySet().asScala ++ incomingDeclaredAddresses)
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
              peerDatabase.blacklistHost(remoteAddress.getAddress)
            } else if (connFuture.isSuccess) {
              log.debug(s"${id(connFuture.channel())} Connection established")
              outgoingChannelCount.incrementAndGet()
              connFuture.channel().closeFuture().addListener { (closeFuture: ChannelFuture) =>
                val remainingCount = outgoingChannelCount.decrementAndGet()
                log.debug(s"${id(closeFuture.channel)} Connection closed, $remainingCount outgoing channel(s) remaining")
                allChannels.remove(closeFuture.channel())
                outgoingChannels.remove(remoteAddress, closeFuture.channel())
              }
              allChannels.add(connFuture.channel())
            }
          }
        }.channel()
    })

  def writeToLocalChannel(message: AnyRef): Unit = localClientChannel.writeAndFlush(message)

  def shutdown(): Unit = try {
    connectTask.cancel(true)
    serverChannel.foreach(_.close().await())
    log.debug("Unbound server")
    allChannels.close().await()
    localClientChannel.close().sync()
    log.debug("Closed all channels")
  } finally {
    workerGroup.shutdownGracefully().await()
    bossGroup.shutdownGracefully().await()
    localClientGroup.shutdownGracefully().await()
    localServerGroup.shutdownGracefully().await()
    coordinatorExecutor.shutdownGracefully().await()
  }
}
