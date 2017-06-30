package com.wavesplatform

import java.io.File
import java.security.Security
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.actor.RootActorSystem
import com.wavesplatform.history.{BlockStorageImpl, CheckpointServiceImpl}
import com.wavesplatform.http.NodeApiRoute
import com.wavesplatform.matcher.Matcher
import com.wavesplatform.mining.Miner
import com.wavesplatform.network.{NetworkServer, PeerDatabaseImpl, PeerInfo, UPnP}
import com.wavesplatform.settings._
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import scorex.account.{Account, AddressScheme}
import scorex.api.http._
import scorex.api.http.alias.{AliasApiRoute, AliasBroadcastApiRoute}
import scorex.api.http.assets.{AssetsApiRoute, AssetsBroadcastApiRoute}
import scorex.api.http.leasing.{LeaseApiRoute, LeaseBroadcastApiRoute}
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.crypto.hash.FastCryptographicHash.DigestSize
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time, TimeImpl}
import scorex.wallet.Wallet
import scorex.waves.http.{DebugApiRoute, WavesApiRoute}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.{Left, Try}

class Application(val actorSystem: ActorSystem, val settings: WavesSettings) extends ScorexLogging {

  private val checkpoints = new CheckpointServiceImpl(settings.blockchainSettings.checkpointFile)
  val (history, stateWriter, stateReader, blockchainUpdater) = BlockStorageImpl(settings.blockchainSettings).get
  private lazy val upnp = new UPnP(settings.networkSettings.uPnPSettings) // don't initialize unless enabled
  private val wallet: Wallet = Wallet(settings.walletSettings)

  def run(): Unit = {
    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")

    checkGenesis()

    if (wallet.privateKeyAccounts().isEmpty)
      wallet.generateNewAccounts(1)

    val feeCalculator = new FeeCalculator(settings.feesSettings)
    val time: Time = new TimeImpl()

    val peerDatabase = new PeerDatabaseImpl(settings.networkSettings)
    val establishedConnections = new ConcurrentHashMap[Channel, PeerInfo]
    val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)

    val utxStorage = new UtxPool(allChannels,
      time, stateReader, feeCalculator, settings.blockchainSettings.functionalitySettings, settings.utxSettings)

    val blockchainReadiness = new AtomicBoolean(false)

    lazy val miner = new Miner(history, stateReader, utxStorage, wallet,
    settings.blockchainSettings, settings.minerSettings, time, allChannels, ???)
//    b => writeToLocalChannel(BlockForged(b)))

    val coordinator: Coordinator = new Coordinator(checkpoints, history, blockchainUpdater, stateReader, utxStorage,
      time, settings.blockchainSettings,
      settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed, settings.checkpointsSettings.publicKey,
      miner, blockchainReadiness)



    val network = new NetworkServer(
      settings.blockchainSettings.addressSchemeCharacter,
      settings.networkSettings.bindAddress,
      settings.networkSettings.declaredAddress,
      settings,
      history,
      coordinator,
      utxStorage,
      peerDatabase,
      allChannels,
      establishedConnections,
      blockchainReadiness)

    miner.lastBlockChanged(history.height(), history.lastBlock)

    val apiRoutes = Seq(
      BlocksApiRoute(settings.restAPISettings, settings.checkpointsSettings, history, network.localClientChannel),
      TransactionsApiRoute(settings.restAPISettings, stateReader, history, utxStorage),
      NxtConsensusApiRoute(settings.restAPISettings, stateReader, history, settings.blockchainSettings.functionalitySettings),
      WalletApiRoute(settings.restAPISettings, wallet),
      PaymentApiRoute(settings.restAPISettings, wallet, utxStorage, time),
      UtilsApiRoute(settings.restAPISettings),
      PeersApiRoute(settings.restAPISettings, network.connect, peerDatabase, establishedConnections),
      AddressApiRoute(settings.restAPISettings, wallet, stateReader, settings.blockchainSettings.functionalitySettings),
      DebugApiRoute(settings.restAPISettings, wallet, stateReader, history, peerDatabase, establishedConnections,
        network.localClientChannel),
      WavesApiRoute(settings.restAPISettings, wallet, utxStorage, time),
      AssetsApiRoute(settings.restAPISettings, wallet, utxStorage, stateReader, time),
      NodeApiRoute(settings.restAPISettings, () => this.shutdown()),
      AssetsBroadcastApiRoute(settings.restAPISettings, utxStorage),
      LeaseApiRoute(settings.restAPISettings, wallet, utxStorage, stateReader, time),
      LeaseBroadcastApiRoute(settings.restAPISettings, utxStorage),
      AliasApiRoute(settings.restAPISettings, wallet, utxStorage, time, stateReader),
      AliasBroadcastApiRoute(settings.restAPISettings, utxStorage)
    )

    val apiTypes = Seq(
      typeOf[BlocksApiRoute],
      typeOf[TransactionsApiRoute],
      typeOf[NxtConsensusApiRoute],
      typeOf[WalletApiRoute],
      typeOf[PaymentApiRoute],
      typeOf[UtilsApiRoute],
      typeOf[PeersApiRoute],
      typeOf[AddressApiRoute],
      typeOf[DebugApiRoute],
      typeOf[WavesApiRoute],
      typeOf[AssetsApiRoute],
      typeOf[NodeApiRoute],
      typeOf[AssetsBroadcastApiRoute],
      typeOf[LeaseApiRoute],
      typeOf[LeaseBroadcastApiRoute],
      typeOf[AliasApiRoute],
      typeOf[AliasBroadcastApiRoute]
    )

    for (addr <- settings.networkSettings.declaredAddress if settings.networkSettings.uPnPSettings.enable) {
      upnp.addPort(addr.getPort)
    }


    implicit val as = actorSystem
    implicit val materializer = ActorMaterializer()

    if (settings.restAPISettings.enable) {
      val combinedRoute: Route = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings.restAPISettings).compositeRoute
      val httpFuture = Http().bindAndHandle(combinedRoute, settings.restAPISettings.bindAddress, settings.restAPISettings.port)
      serverBinding = Await.result(httpFuture, 10.seconds)
      log.info(s"REST API was bound on ${settings.restAPISettings.bindAddress}:${settings.restAPISettings.port}")
    }

    //on unexpected shutdown
    sys.addShutdownHook {
      network.shutdown()
      shutdown()
    }

    if (settings.matcherSettings.enable) {
      val matcher = new Matcher(actorSystem, wallet, utxStorage, stateReader, time, history, settings.blockchainSettings, settings.restAPISettings, settings.matcherSettings)
      matcher.runMatcher()
    }
  }

  def checkGenesis(): Unit = {
    if (history.isEmpty) {
      val maybeGenesisSignature = Option(settings.blockchainSettings.genesisSettings.signature).filter(_.trim.nonEmpty)
      Block.genesis(
        NxtLikeConsensusBlockData(settings.blockchainSettings.genesisSettings.initialBaseTarget, Array.fill(DigestSize)(0: Byte)),
        Application.genesisTransactions(settings.blockchainSettings.genesisSettings),
        settings.blockchainSettings.genesisSettings.blockTimestamp, maybeGenesisSignature)
        .flatMap(blockchainUpdater.processBlock) match {
        case Left(value) =>
          log.error(value.toString)
          System.exit(1)
        case _ =>
      }

      log.info("Genesis block has been added to the state")
    }
  }

  @volatile var shutdownInProgress = false
  @volatile var serverBinding: ServerBinding = _

  def shutdown(): Unit = {
    if (!shutdownInProgress) {
      log.info("Stopping network services")
      shutdownInProgress = true
      if (settings.restAPISettings.enable) {
        Try(Await.ready(serverBinding.unbind(), 60.seconds)).failed.map(e => log.error("Failed to unbind REST API port: " + e.getMessage))
      }
      for (addr <- settings.networkSettings.declaredAddress if settings.networkSettings.uPnPSettings.enable) {
        upnp.deletePort(addr.getPort)
      }

      implicit val askTimeout = Timeout(60.seconds)
      Try(Await.result(actorSystem.terminate(), 60.seconds))
        .failed.map(e => log.error("Failed to terminate actor system: " + e.getMessage))
      log.debug("Closing storage")
      wallet.close()
      stateWriter.close()
      history.close()
      log.info("Shutdown complete")
    }
  }

}

object Application extends ScorexLogging {

  private def configureLogging(settings: WavesSettings) = {
    import ch.qos.logback.classic.{Level, LoggerContext}
    import org.slf4j._

    val lc = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val rootLogger = lc.getLogger(Logger.ROOT_LOGGER_NAME)
    settings.loggingLevel match {
      case LogLevel.TRACE => rootLogger.setLevel(Level.TRACE)
      case LogLevel.DEBUG => rootLogger.setLevel(Level.DEBUG)
      case LogLevel.INFO => rootLogger.setLevel(Level.INFO)
      case LogLevel.WARN => rootLogger.setLevel(Level.WARN)
      case LogLevel.ERROR => rootLogger.setLevel(Level.ERROR)
    }
  }

  private def readConfig(userConfigPath: Option[String]): Config = {
    val maybeConfigFile = for {
      maybeFilename <- userConfigPath
      file = new File(maybeFilename)
      if file.exists
    } yield file

    val config = maybeConfigFile match {
      // if no user config is supplied, the library will handle overrides/application/reference automatically
      case None =>
        log.warn("NO CONFIGURATION FILE WAS PROVIDED. STARTING WITH DEFAULT SETTINGS FOR TESTNET!")
        ConfigFactory.load()
      // application config needs to be resolved wrt both system properties *and* user-supplied config.
      case Some(file) =>
        val cfg = ConfigFactory.parseFile(file)
        if (!cfg.hasPath("waves")) {
          log.error("Malformed configuration file was provided! Aborting!")
          log.error("Please, read following article about configuration file format:")
          log.error("https://github.com/wavesplatform/Waves/wiki/Waves-Node-configuration-file")
          System.exit(1)
        }
        loadConfig(cfg)
    }

    config
  }

  def main(args: Array[String]): Unit = {
    // prevents java from caching successful name resolutions, which is needed e.g. for proper NTP server rotation
    // http://stackoverflow.com/a/17219327
    Security.setProperty("networkaddress.cache.ttl", "0")

    log.info("Starting...")

    val config = readConfig(args.headOption)
    val settings = WavesSettings.fromConfig(config)

    RootActorSystem.start("wavesplatform", settings.matcherSettings) { actorSystem =>
      configureLogging(settings)

      // Initialize global var with actual address scheme
      AddressScheme.current = new AddressScheme {
        override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
      }

      log.info(s"${Constants.AgentName} Blockchain Id: ${settings.blockchainSettings.addressSchemeCharacter}")

      new Application(actorSystem, settings).run()
    }
  }

  def genesisTransactions(gs: GenesisSettings): Seq[GenesisTransaction] = {
    gs.transactions.map { ts =>
      val acc = Account.fromString(ts.recipient).right.get
      GenesisTransaction.create(acc, ts.amount, gs.transactionsTimestamp).right.get
    }
  }

}
