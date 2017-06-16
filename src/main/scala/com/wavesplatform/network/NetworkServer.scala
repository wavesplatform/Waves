package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress, NetworkInterface}
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
import io.netty.channel.group.{ChannelGroup, ChannelMatchers}
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

  private val network = new Network {
    override def requestExtension(localScore: BigInt): Unit = broadcast(LocalScoreChanged(localScore))
    override def broadcast(msg: AnyRef, except: Option[Channel]): Unit = doBroadcast(msg, except)
  }

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

  println(excludedAddresses.mkString("\n"))

  private val lengthFieldPrepender = new LengthFieldPrepender(4)

  private val miner = new Miner(history, stateReader, utxStorage, wallet.privateKeyAccounts(),
    settings.blockchainSettings, settings.minerSettings, time.correctedTime(), allChannels.size(),
    b => writeToLocalChannel(BlockForged(b)))

  private val peerSynchronizer = new PeerSynchronizer(peerDatabase)
  private val utxPoolSynchronizer = new UtxPoolSynchronizer(txHandler, network)
  private val errorHandler = new ErrorHandler(blacklist)
  private val historyReplier = new HistoryReplier(history, settings.synchronizationSettings.maxChainLength)

  private val inboundConnectionFilter = new InboundConnectionFilter(peerDatabase,
    settings.networkSettings.maxInboundConnections,
    settings.networkSettings.maxConnectionsPerHost)

  private val coordinatorExecutor = new DefaultEventLoop
  private val coordinator = new Coordinator(checkpoints, history, blockchainUpdater, stateReader, utxStorage,
    time.correctedTime(), settings.blockchainSettings,
    settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed, settings.checkpointsSettings.publicKey,
    miner, setBlockchainExpired)

  private val coordinatorHandler = new CoordinatorHandler(coordinator)

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
  private val localClientChannel = new Bootstrap()
    .group(localClientGroup)
    .channel(classOf[LocalChannel])
    .handler(new PipelineInitializer[LocalChannel](Seq.empty))
    .connect(address)
    .channel()

  log.info(s"${id(localClientChannel)} Local channel opened")

  private val peerUniqueness = new ConcurrentHashMap[PeerKey, Channel]()

  private val serverHandshakeHandler =
    new HandshakeHandler.Server(handshake, peerInfo, peerUniqueness, blacklist, allChannels)

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
        new LengthFieldBasedFrameDecoder(1024*1024, 0, 4, 0, 4),
        new LegacyFrameCodec,
        discardingHandler,
        messageCodec,
        peerSynchronizer,
        historyReplier,
        new ExtensionSignaturesLoader(settings.synchronizationSettings.synchronizationTimeout, blacklist),
        new ExtensionBlocksLoader(history, settings.synchronizationSettings.synchronizationTimeout, blacklist),
        new OptimisticExtensionLoader,
        utxPoolSynchronizer,
        scoreObserver,
        coordinatorHandler -> coordinatorExecutor)))
      .bind(bindAddress)
      .channel()
  }

  private val outgoingChannelCount = new AtomicInteger(0)
  private val channels = new ConcurrentHashMap[InetSocketAddress, Channel]

  private val clientHandshakeHandler =
    new HandshakeHandler.Client(handshake, peerInfo, peerUniqueness, blacklist)

  private val bootstrap = new Bootstrap()
    .group(workerGroup)
    .channel(classOf[NioSocketChannel])
    .handler(new PipelineInitializer[SocketChannel](Seq(
      errorHandler,
      new HandshakeDecoder,
      new HandshakeTimeoutHandler,
      clientHandshakeHandler,
      lengthFieldPrepender,
      new LengthFieldBasedFrameDecoder(1024*1024, 0, 4, 0, 4),
      new LegacyFrameCodec,
      discardingHandler,
      messageCodec,
      peerSynchronizer,
      historyReplier,
      new ExtensionSignaturesLoader(settings.synchronizationSettings.synchronizationTimeout, blacklist),
      new ExtensionBlocksLoader(history, settings.synchronizationSettings.synchronizationTimeout, blacklist),
      new OptimisticExtensionLoader,
      utxPoolSynchronizer,
      scoreObserver,
      coordinatorHandler -> coordinatorExecutor)))

  workerGroup.scheduleWithFixedDelay(1.second, 5.seconds) {
    if (outgoingChannelCount.get() < settings.networkSettings.maxOutboundConnections) {
      peerDatabase
        .getRandomPeer(excludedAddresses ++ channels.keySet().asScala)
        .foreach(connect)
    }
  }

  def connect(remoteAddress: InetSocketAddress): Unit =
    channels.computeIfAbsent(remoteAddress, _ => {
      bootstrap.connect(remoteAddress)
        .addListener { (connFuture: ChannelFuture) =>
          if (connFuture.isDone) {
            if (connFuture.cause() != null) {
              log.debug(s"${id(connFuture.channel())} Connection failed, blacklisting $remoteAddress", connFuture.cause())
              blacklist(remoteAddress.getAddress)
            } else if (connFuture.isSuccess) {
              log.debug(s"${id(connFuture.channel())} Connection established")
              outgoingChannelCount.incrementAndGet()
              connFuture.channel().closeFuture().addListener { (closeFuture: ChannelFuture) =>
                val remainingCount = outgoingChannelCount.decrementAndGet()
                log.debug(s"${id(closeFuture.channel)} Connection closed, $remainingCount outgoing channel(s) remaining")
                allChannels.remove(closeFuture.channel())
                channels.remove(remoteAddress, closeFuture.channel())
              }
              allChannels.add(connFuture.channel())
            }
          }
        }.channel()
    })

  def writeToLocalChannel(message: AnyRef): Unit = localClientChannel.writeAndFlush(message)

  private def doBroadcast(message: AnyRef, except: Option[Channel] = None): Unit = {
    log.trace(s"Broadcasting $message to ${allChannels.size()} channels${except.fold("")(c => s" (except ${id(c)})")}")
    allChannels.writeAndFlush(message, except.fold(ChannelMatchers.all())(ChannelMatchers.isNot))
  }

  private def blacklist(address: InetAddress): Unit = {
    log.debug(s"Blacklisting $address")
    peerDatabase.blacklistHost(address)
  }

  private def blacklist(channel: Channel): Unit = {
    blacklist(channel.asInstanceOf[NioSocketChannel].remoteAddress().getAddress)
    channel.close()
  }

  def shutdown(): Unit = try {
    serverChannel.foreach(_.close().await())
    log.debug("Unbound server")
    allChannels.close().await()
    log.debug("Closed all channels")
  } finally {
    workerGroup.shutdownGracefully()
    bossGroup.shutdownGracefully()
    localClientGroup.shutdownGracefully()
    localServerGroup.shutdownGracefully()
    coordinatorExecutor.shutdownGracefully()
  }
}
