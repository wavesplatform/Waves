package com.wavesplatform.network

import java.net.{InetSocketAddress, NetworkInterface}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

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
import scorex.network.TransactionalMessagesRepo
import scorex.network.message.{BasicMessagesRepo, MessageSpec}
import scorex.network.peer.{PeerDatabase, PeerDatabaseImpl}
import scorex.transaction._
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

class NetworkServer(
    chainId: Char,
    settings: WavesSettings,
    history: History,
    checkpoints: CheckpointService,
    blockchainUpdater: BlockchainUpdater,
    time: Time,
    stateReader: StateReader,
    utxStorage: UnconfirmedTransactionsStorage,
    txHandler: NewTransactionHandler,
    allChannels: ChannelGroup) extends ScorexLogging {
  private val bossGroup = new NioEventLoopGroup()
  private val workerGroup = new NioEventLoopGroup()
  private val handshake =
    Handshake(Constants.ApplicationName + chainId, Version.VersionTuple, settings.networkSettings.nodeName,
      settings.networkSettings.nonce, settings.networkSettings.declaredAddress)

  private val allConnectedPeers = new ConcurrentHashMap[PeerKey, Channel]
  private val scoreObserver = new RemoteScoreObserver(settings.synchronizationSettings)

  private val allLocalInterfaces = (for {
    ifc <-NetworkInterface.getNetworkInterfaces.asScala
    ip <- ifc.getInterfaceAddresses.asScala
  } yield new InetSocketAddress(ip.getAddress, settings.networkSettings.port)).toSet

  private val peerDatabase = new PeerDatabaseImpl(settings.networkSettings)

  private val discardingHandler = new DiscardingHandler
  private val specs: Map[Byte, MessageSpec[_ <: AnyRef]] = (BasicMessagesRepo.specs ++ TransactionalMessagesRepo.specs).map(s => s.messageCode -> s).toMap
  private val messageCodec = new MessageCodec(specs)

  private val network = new Network {
    override def requestExtension(localScore: BigInt) = broadcast(LocalScoreChanged(localScore))
    override def broadcast(msg: AnyRef, except: Option[Channel]) = doBroadcast(msg, except)
  }

  private val coordinatorExecutor = new DefaultEventLoop
  private val coordinator = new Coordinator(checkpoints, history, blockchainUpdater, time, stateReader, utxStorage, settings.blockchainSettings, network)

  private val bootstrap = new Bootstrap()
    .group(workerGroup)
    .channel(classOf[NioSocketChannel])
    .handler(new ChannelInitializer[SocketChannel] {
      override def initChannel(ch: SocketChannel): Unit =
        ch.pipeline()
          .addLast(
            new HandshakeDecoder,
            new HandshakeTimeoutHandler,
            new ClientHandshakeHandler(handshake, allConnectedPeers),
            new LengthFieldPrepender(4),
            new LengthFieldBasedFrameDecoder(1024*1024, 0, 4, 0, 4),
            new LegacyFrameCodec,
            discardingHandler,
            messageCodec,
            new PeerSynchronizer(peerDatabase),
            new ExtensionSignaturesLoader(history, settings.synchronizationSettings),
            new ExtensionBlocksLoader(history, settings.synchronizationSettings.synchronizationTimeout),
            new UtxPoolSynchronizer(txHandler, network),
            scoreObserver)
          .addLast(coordinatorExecutor, coordinator)
    })

  private val serverChannel = new ServerBootstrap()
    .group(bossGroup, workerGroup)
    .channel(classOf[NioServerSocketChannel])
    .childHandler(new ServerChannelInitializer(handshake, settings.networkSettings, peerDatabase))
    .bind(settings.networkSettings.port)
    .channel()

  workerGroup.scheduleWithFixedDelay(settings.synchronizationSettings.scoreBroadcastInterval,
    settings.synchronizationSettings.scoreBroadcastInterval) {
    doBroadcast(history.score())
  }

  private val outgoingChannelCount = new AtomicInteger(0)

  private val channels = new ConcurrentHashMap[InetSocketAddress, Channel]

  workerGroup.scheduleWithFixedDelay(1.second, 5.seconds) {
    if (outgoingChannelCount.get() < settings.networkSettings.maxOutboundConnections) {
      peerDatabase.getRandomPeer(allLocalInterfaces ++ channels.keySet().asScala).foreach(connect)
    }
  }

  private def connect(remoteAddress: InetSocketAddress): Channel = {
    channels.computeIfAbsent(remoteAddress, _ => {
      outgoingChannelCount.incrementAndGet()
      val chan = bootstrap.connect(remoteAddress).channel()
      allChannels.add(chan)
      log.debug(s"Connecting ${chan.id().asShortText()} to $remoteAddress")
      chan.closeFuture().addListener { (chf: ChannelFuture) =>
        val remainingOutgoingChannelCount = outgoingChannelCount.decrementAndGet()
        log.debug(s"Connection to $remoteAddress (channel ${chf.channel().id().asShortText()}) is closed, $remainingOutgoingChannelCount channel(s) remaining")
        allChannels.remove(chf.channel())
        channels.remove(remoteAddress, chf.channel())
      }
      chan
    })
  }

  private def doBroadcast(message: AnyRef, except: Option[Channel] = None): Unit = {
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
