package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress, NetworkInterface}
import java.util.concurrent.ConcurrentHashMap
import java.util.stream.Collectors

import com.wavesplatform.Version
import com.wavesplatform.settings._
import com.wavesplatform.state2.reader.StateReader
import io.netty.bootstrap.{Bootstrap, ServerBootstrap}
import io.netty.channel._
import io.netty.channel.group.{ChannelGroup, ChannelMatchers}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}
import io.netty.util.concurrent.EventExecutorGroup
import scorex.network.TransactionalMessagesRepo
import scorex.network.message.{BasicMessagesRepo, MessageSpec}
import scorex.network.peer.{PeerDatabase, PeerDatabaseImpl}
import scorex.transaction.{BlockchainUpdater, CheckpointService, History, UnconfirmedTransactionsStorage}
import scorex.utils.{ScorexLogging, Time}

import scala.collection.JavaConverters._
import scala.concurrent.duration._

class ServerChannelInitializer(handshake: Handshake, settings: NetworkSettings, peerDatabase: PeerDatabase)
  extends ChannelInitializer[SocketChannel] {
  private val inboundFilter = new InboundConnectionFilter(peerDatabase, settings.maxInboundConnections, settings.maxConnectionsWithSingleHost)
  override def initChannel(ch: SocketChannel): Unit = {
    ch.pipeline()
      .addLast(inboundFilter)
      .addLast(HandshakeDecoder.Name, new HandshakeDecoder)
      .addLast(HandshakeTimeoutHandler.Name, new HandshakeTimeoutHandler)
  }
}

class ClientChannelInitializer(
    handshake: Handshake,
    history: History,
    checkpoints: CheckpointService,
    blockchainUpdater: BlockchainUpdater,
    time: Time,
    stateReader: StateReader,
    utxStorage: UnconfirmedTransactionsStorage,
    syncSettings: SynchronizationSettings,
    networkSettings: NetworkSettings,
    network: Network,
    peerDatabase: PeerDatabase,
    connections: ConcurrentHashMap[PeerKey, Channel],
    scoreObserver: RemoteScoreObserver,
    coordinatorEventLoop: EventExecutorGroup,
    coordinator: Coordinator)
  extends ChannelInitializer[SocketChannel] {

  private val specs: Map[Byte, MessageSpec[_ <: AnyRef]] = (BasicMessagesRepo.specs ++ TransactionalMessagesRepo.specs).map(s => s.messageCode -> s).toMap

  override def initChannel(ch: SocketChannel): Unit = {
    ch.pipeline()
      .addLast(HandshakeDecoder.Name, new HandshakeDecoder)
      .addLast(HandshakeTimeoutHandler.Name, new HandshakeTimeoutHandler)
      .addLast(ClientHandshakeHandler.Name, new ClientHandshakeHandler(handshake, connections))
      .addLast(new LengthFieldPrepender(4))
      .addLast(new LengthFieldBasedFrameDecoder(1024*1024, 0, 4, 0, 4))
      .addLast(new MessageCodec(specs))
      .addLast(new PeerSynchronizer(peerDatabase))
      .addLast(new ExtensionSignaturesLoader(history, syncSettings))
      .addLast(new ExtensionBlocksLoader(history, syncSettings.synchronizationTimeout))
      .addLast(scoreObserver)
      .addLast(coordinatorEventLoop, coordinator)
  }
}

trait Network {
  def requestExtension(localScore: BigInt): Unit
}

class NetworkServer(
    chainId: Char,
    settings: WavesSettings,
    history: History,
    checkpoints: CheckpointService,
    blockchainUpdater: BlockchainUpdater,
    time: Time,
    stateReader: StateReader,
    utxStorage: UnconfirmedTransactionsStorage,
    allChannels: ChannelGroup) extends ScorexLogging {
  private val bossGroup = new NioEventLoopGroup()
  private val workerGroup = new NioEventLoopGroup()
  private val handshake =
    Handshake(Constants.ApplicationName + chainId, Version.VersionTuple, settings.networkSettings.nodeName,
      settings.networkSettings.nonce, settings.networkSettings.declaredAddress)

  private val allConnectedPeers = new ConcurrentHashMap[PeerKey, Channel]
  private val knownPeers = settings.networkSettings.knownPeers.map(inetSocketAddress(_, 6863)).toSet
  private val scoreObserver = new RemoteScoreObserver(settings.synchronizationSettings)

  private def connectedPeerAddresses =
    allConnectedPeers.keySet.stream.map[InetAddress](pk => pk.host).collect(Collectors.toSet())

  private val allLocalInterfaces = (for {
    ifc <-NetworkInterface.getNetworkInterfaces.asScala
    ip <- ifc.getInterfaceAddresses.asScala
  } yield new InetSocketAddress(ip.getAddress, settings.networkSettings.port)).toSet

  private val peerDatabase = {
    val db = new PeerDatabaseImpl(settings.networkSettings, Some(settings.networkSettings.file))

    for (a <- settings.networkSettings.knownPeers.view.map(inetSocketAddress(_, 6863))) {
      db.addPeer(a, None, None)
    }

    db
  }

  private val network = new Network {
    override def requestExtension(localScore: BigInt) =
      allChannels.writeAndFlush(LocalScoreChanged(localScore))
  }

  private val coordinatorExecutor = new DefaultEventLoop
  private val coordinator = new Coordinator(checkpoints, history, blockchainUpdater, time, stateReader, utxStorage, settings.blockchainSettings, network)

  private val bootstrap = new Bootstrap()
    .group(workerGroup)
    .channel(classOf[NioSocketChannel])
    .handler(new ClientChannelInitializer(
      handshake,
      history,
      checkpoints,
      blockchainUpdater,
      time,
      stateReader,
      utxStorage,
      settings.synchronizationSettings,
      settings.networkSettings,
      network,
      peerDatabase,
      allConnectedPeers,
      scoreObserver,
      coordinatorExecutor,
      coordinator))

  private val serverChannel = new ServerBootstrap()
    .group(bossGroup, workerGroup)
    .channel(classOf[NioServerSocketChannel])
    .childHandler(new ServerChannelInitializer(handshake, settings.networkSettings, peerDatabase))
    .bind(settings.networkSettings.port)
    .channel()

  workerGroup.scheduleWithFixedDelay(1.second, 5.seconds) {
    val inactiveConnections = allChannels.stream().filter(!_.isActive).collect(Collectors.toSet())
    inactiveConnections.forEach(allChannels.remove _)

    if (allChannels.size() < settings.networkSettings.maxOutboundConnections) {
      peerDatabase.getRandomPeer(allLocalInterfaces).foreach(connect)
    }
  }

  workerGroup.scheduleWithFixedDelay(settings.synchronizationSettings.scoreBroadcastInterval,
    settings.synchronizationSettings.scoreBroadcastInterval) {
    broadcast(history.score())
  }

  private def connect(remoteAddress: InetSocketAddress): ChannelFuture = {
    val f = bootstrap.connect(remoteAddress)
    log.debug(s"Connecting to $remoteAddress ${f.channel().id().asShortText()}")
    allChannels.add(f.channel())
    f
  }

  def broadcast(message: AnyRef, except: Option[Channel] = None): Unit = {
    log.debug(s"Broadcasting $message to ${allChannels.size()} channels${except.fold("")(c => s" (except ${c.id().asShortText()})")}")
    allChannels.writeAndFlush(message, except.fold(ChannelMatchers.all())(ChannelMatchers.isNot))
  }

  def shutdown(): Unit = try {
    serverChannel.close().await()
    log.debug("Unbound server")
    allChannels.close().await()
    log.debug("Closed all channels")
  } finally {
    workerGroup.shutdownGracefully()
    bossGroup.shutdownGracefully()
    coordinatorExecutor.shutdownGracefully()
  }
}
