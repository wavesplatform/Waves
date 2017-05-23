package com.wavesplatform

import java.io.File
import java.security.Security

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.actor.RootActorSystem
import com.wavesplatform.history.BlockStorageImpl
import com.wavesplatform.http.NodeApiRoute
import com.wavesplatform.matcher.Matcher
import com.wavesplatform.settings._
import scorex.account.{Account, AddressScheme}
import scorex.api.http._
import scorex.api.http.alias.{AliasApiRoute, AliasBroadcastApiRoute}
import scorex.api.http.assets.{AssetsApiRoute, AssetsBroadcastApiRoute}
import scorex.api.http.leasing.{LeaseApiRoute, LeaseBroadcastApiRoute}
import scorex.block.Block
import scorex.consensus.mining.BlockGeneratorController
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash.DigestSize
import scorex.network._
import scorex.network.message.{BasicMessagesRepo, MessageHandler}
import scorex.network.peer.PeerManager
import scorex.transaction._
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.utils.{ScorexLogging, Time, TimeImpl}
import scorex.wallet.Wallet
import scorex.waves.http.{DebugApiRoute, WavesApiRoute}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.{Left, Try}

class Application(val actorSystem: ActorSystem, val settings: WavesSettings) extends ScorexLogging {

  lazy val upnp = new UPnP(settings.networkSettings.uPnPSettings)

  val messagesHandler = MessageHandler(BasicMessagesRepo.specs ++ TransactionalMessagesRepo.specs)
  val feeCalculator = new FeeCalculator(settings.feesSettings)
  val (checkpoints, history, stateReader, blockchainUpdater) = BlockStorageImpl(settings.blockchainSettings)
  val time: Time = new TimeImpl()
  val wallet: Wallet = {
    val maybeWalletFilename = Option(settings.walletSettings.file).filter(_.trim.nonEmpty)
    val seed = Base58.decode(settings.walletSettings.seed).toOption
    new Wallet(maybeWalletFilename, settings.walletSettings.password, seed)
  }
  val utxStorage: UnconfirmedTransactionsStorage = new UnconfirmedTransactionsDatabaseImpl(settings.utxSettings.size)
  val newTransactionHandler = new NewTransactionHandlerImpl(settings.blockchainSettings.functionalitySettings,
    networkController, feeCalculator, utxStorage, history, stateReader)

  lazy val apiRoutes = Seq(
    BlocksApiRoute(settings.restAPISettings, settings.checkpointsSettings, history, coordinator),
    TransactionsApiRoute(settings.restAPISettings, stateReader, history, utxStorage),
    NxtConsensusApiRoute(settings.restAPISettings, stateReader, history, settings.blockchainSettings.functionalitySettings),
    WalletApiRoute(settings.restAPISettings, wallet),
    PaymentApiRoute(settings.restAPISettings, wallet, newTransactionHandler, time),
    UtilsApiRoute(settings.restAPISettings),
    PeersApiRoute(settings.restAPISettings, peerManager, networkController),
    AddressApiRoute(settings.restAPISettings, wallet, stateReader, settings.blockchainSettings.functionalitySettings),
    DebugApiRoute(settings.restAPISettings, wallet, stateReader, blockchainUpdater, history),
    WavesApiRoute(settings.restAPISettings, wallet, newTransactionHandler, time),
    AssetsApiRoute(settings.restAPISettings, wallet, stateReader, newTransactionHandler, time),
    NodeApiRoute(settings.restAPISettings, () => this.shutdown(), blockGenerator, coordinator),
    AssetsBroadcastApiRoute(settings.restAPISettings, newTransactionHandler),
    LeaseApiRoute(settings.restAPISettings, wallet, stateReader, newTransactionHandler, time),
    LeaseBroadcastApiRoute(settings.restAPISettings, newTransactionHandler),
    AliasApiRoute(settings.restAPISettings, wallet, newTransactionHandler, time, stateReader),
    AliasBroadcastApiRoute(settings.restAPISettings, newTransactionHandler)
  )

  lazy val apiTypes = Seq(
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

  lazy val networkController: ActorRef = actorSystem.actorOf(Props(new NetworkController(settings.networkSettings, upnp, peerManager, messagesHandler)), "NetworkController")
  lazy val peerManager: ActorRef = actorSystem.actorOf(PeerManager.props(settings.networkSettings, networkController, settings.blockchainSettings.addressSchemeCharacter), "PeerManager")
  lazy val unconfirmedPoolSynchronizer: ActorRef = actorSystem.actorOf(Props(new UnconfirmedPoolSynchronizer(newTransactionHandler, settings.utxSettings, networkController, utxStorage)))
  lazy val coordinator: ActorRef = actorSystem.actorOf(Props(new Coordinator(networkController, blockchainSynchronizer, blockGenerator, peerManager, scoreObserver, blockchainUpdater, time, utxStorage, history, stateReader, checkpoints, settings)), "Coordinator")
  lazy val scoreObserver: ActorRef = actorSystem.actorOf(Props(new ScoreObserver(networkController, coordinator, settings.synchronizationSettings)), "ScoreObserver")
  lazy val historyReplier: ActorRef = actorSystem.actorOf(Props(new HistoryReplier(networkController, history: History, settings.synchronizationSettings.maxChainLength: Int)), "HistoryReplier")
  lazy val blockGenerator: ActorRef = actorSystem.actorOf(Props(new BlockGeneratorController(settings.minerSettings, history, time, peerManager,
    wallet, stateReader, settings.blockchainSettings, utxStorage, coordinator)), "BlockGenerator")
  lazy val blockchainSynchronizer: ActorRef = actorSystem.actorOf(Props(new BlockchainSynchronizer(networkController, coordinator, history, settings.synchronizationSettings)), "BlockchainSynchronizer")
  lazy val peerSynchronizer: ActorRef = actorSystem.actorOf(Props(new PeerSynchronizer(networkController, peerManager, settings.networkSettings)), "PeerSynchronizer")

  def run(): Unit = {
    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")

    checkGenesis()

    if (settings.networkSettings.uPnPSettings.enable) upnp.addPort(settings.networkSettings.port)


    implicit val as = actorSystem
    implicit val materializer = ActorMaterializer()

    if (settings.restAPISettings.enable) {
      val combinedRoute: Route = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings.restAPISettings).compositeRoute
      val httpFuture = Http().bindAndHandle(combinedRoute, settings.restAPISettings.bindAddress, settings.restAPISettings.port)
      serverBinding = Await.result(httpFuture, 10.seconds)
      log.info(s"REST API was bound on ${settings.restAPISettings.bindAddress}:${settings.restAPISettings.port}")
    }

    Seq(scoreObserver, blockGenerator, blockchainSynchronizer, historyReplier, coordinator, unconfirmedPoolSynchronizer, peerSynchronizer) foreach {
      _ => // de-lazyning process :-)
    }


    //on unexpected shutdown
    sys.addShutdownHook {
      shutdown()
    }

    if (settings.matcherSettings.enable) {
      val matcher = new Matcher(actorSystem, wallet, newTransactionHandler, stateReader, history, settings.blockchainSettings, settings.restAPISettings, settings.matcherSettings)
      matcher.runMatcher()
    }
  }

  def checkGenesis(): Unit = {
    if (history.isEmpty) {
      val maybeGenesisSignature = Option(settings.blockchainSettings.genesisSettings.signature).filter(_.trim.nonEmpty)
      blockchainUpdater.processBlock(Block.genesis(
        NxtLikeConsensusBlockData(settings.blockchainSettings.genesisSettings.initialBaseTarget, Array.fill(DigestSize)(0: Byte)),
        Application.genesisTransactions(settings.blockchainSettings.genesisSettings),
        settings.blockchainSettings.genesisSettings.blockTimestamp, maybeGenesisSignature)) match {
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
      if (settings.networkSettings.uPnPSettings.enable) upnp.deletePort(settings.networkSettings.port)

      implicit val askTimeout = Timeout(60.seconds)
      Try(Await.result(networkController ? NetworkController.ShutdownNetwork, 60.seconds))
        .failed.map(e => log.error("Failed to shutdown network: " + e.getMessage))
      Try(Await.result(actorSystem.terminate(), 60.seconds))
        .failed.map(e => log.error("Failed to terminate actor system: " + e.getMessage))
      log.debug("Closing wallet")
      wallet.close()
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
        ConfigFactory
          .defaultOverrides()
          .withFallback(cfg)
          .withFallback(ConfigFactory.defaultApplication())
          .withFallback(ConfigFactory.defaultReference())
          .resolve()
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

      val application = new Application(actorSystem, settings)
      application.run()

      if (application.wallet.privateKeyAccounts().isEmpty) {
        application.wallet.generateNewAccounts(1)
      }
    }
  }

  def genesisTransactions(gs: GenesisSettings): Seq[GenesisTransaction] = {
    gs.transactions.map { ts =>
      val acc = Account.fromString(ts.recipient).right.get
      GenesisTransaction.create(acc, ts.amount, gs.transactionsTimestamp).right.get
    }
  }
  def f00: Int = 4

}
