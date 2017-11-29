package com.wavesplatform

import java.io.File
import java.security.Security
import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import cats.instances.all._
import com.typesafe.config._
import com.wavesplatform.actor.RootActorSystem
import com.wavesplatform.db.openDB
import com.wavesplatform.features.api.ActivationApiRoute
import com.wavesplatform.history.{CheckpointServiceImpl, StorageFactory}
import com.wavesplatform.http.NodeApiRoute
import com.wavesplatform.matcher.Matcher
import com.wavesplatform.metrics.Metrics
import com.wavesplatform.mining.{Miner, MinerImpl}
import com.wavesplatform.network._
import com.wavesplatform.settings._
import com.wavesplatform.state2.appender.{BlockAppender, CheckpointAppender, ExtensionAppender, MicroblockAppender}
import com.wavesplatform.utils.{SystemInformationReporter, forceStopApplication}
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import kamon.Kamon
import monix.reactive.Observable
import org.influxdb.dto.Point
import org.slf4j.bridge.SLF4JBridgeHandler
import scorex.account.AddressScheme
import scorex.api.http._
import scorex.api.http.alias.{AliasApiRoute, AliasBroadcastApiRoute}
import scorex.api.http.assets.{AssetsApiRoute, AssetsBroadcastApiRoute}
import scorex.api.http.leasing.{LeaseApiRoute, LeaseBroadcastApiRoute}
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.transaction._
import scorex.utils.{NTP, ScorexLogging, Time}
import scorex.wallet.Wallet
import scorex.waves.http.{DebugApiRoute, WavesApiRoute}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.Try

class Application(val actorSystem: ActorSystem, val settings: WavesSettings, configRoot: ConfigObject) extends ScorexLogging {

  private val db = openDB(settings.dataDirectory)

  private val LocalScoreBroadcastDebounce = 1.second

  private val checkpointService = new CheckpointServiceImpl(settings.blockchainSettings.checkpointFile, settings.checkpointsSettings)
  private val (history, featureProvider, stateWriter, stateReader, blockchainUpdater, blockchainDebugInfo) = StorageFactory(settings).get
  private lazy val upnp = new UPnP(settings.networkSettings.uPnPSettings) // don't initialize unless enabled
  private val wallet: Wallet = Wallet(settings.walletSettings)
  private val peerDatabase = new PeerDatabaseImpl(settings.networkSettings)

  def run(): Unit = {
    checkGenesis(history, settings, blockchainUpdater)

    if (wallet.privateKeyAccounts.isEmpty)
      wallet.generateNewAccounts(1)

    val feeCalculator = new FeeCalculator(settings.feesSettings)
    val time: Time = NTP

    val peerDatabase = new PeerDatabaseImpl(db, settings.networkSettings)
    val establishedConnections = new ConcurrentHashMap[Channel, PeerInfo]
    val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    val utxStorage = new UtxPoolImpl(time, stateReader, history, feeCalculator, settings.blockchainSettings.functionalitySettings, settings.utxSettings)
    val knownInvalidBlocks = new InvalidBlockStorageImpl(settings.synchronizationSettings.invalidBlocksStorage)
    val miner = if (settings.minerSettings.enable)
      new MinerImpl(allChannels, blockchainUpdater, checkpointService, history, featureProvider, stateReader, settings, time, utxStorage, wallet)
    else Miner.Disabled

    val processBlock = BlockAppender(checkpointService, history, blockchainUpdater, time, stateReader, utxStorage, settings.blockchainSettings, featureProvider, allChannels, peerDatabase, miner) _
    val processCheckpoint = CheckpointAppender(checkpointService, history, blockchainUpdater, peerDatabase, miner, allChannels) _
    val processFork = ExtensionAppender(checkpointService, history, blockchainUpdater, stateReader, utxStorage, time, settings, featureProvider, knownInvalidBlocks, peerDatabase, miner, allChannels) _
    val processMicroBlock = MicroblockAppender(checkpointService, history, blockchainUpdater, utxStorage, allChannels, peerDatabase) _

    import blockchainUpdater.lastBlockInfo

    val lastScore = lastBlockInfo
      .map(_.score)
      .distinctUntilChanged
      .debounce(LocalScoreBroadcastDebounce)
      .share(scheduler)

    lastScore.foreach { x =>
      allChannels.broadcast(LocalScoreChanged(x))
    }(scheduler)

    val network = NetworkServer(settings, lastBlockInfo, history, utxStorage, peerDatabase, allChannels, establishedConnections)
    val (signatures, blocks, blockchainScores, checkpoints, microblockInvs, microblockResponses) = network.messages

    val syncWithChannelClosed = RxScoreObserver(settings.synchronizationSettings.scoreTTL, history.score(), lastScore, blockchainScores, network.closedChannels)
    val microblockDatas = MicroBlockSynchronizer(settings.synchronizationSettings.microBlockSynchronizer, history, peerDatabase)(lastBlockInfo.map(_.id), microblockInvs, microblockResponses)
    val newBlocks = RxExtensionLoader(settings.synchronizationSettings.maxRollback, settings.synchronizationSettings.synchronizationTimeout,
      history, peerDatabase, knownInvalidBlocks, blocks, signatures, syncWithChannelClosed) { case ((c, b)) => processFork(c, b.blocks) }

    val microblockSink = microblockDatas.mapTask(scala.Function.tupled(processMicroBlock))
    val blockSink = newBlocks.mapTask(scala.Function.tupled(processBlock))
    val checkpointSink = checkpoints.mapTask { case ((s, c)) => processCheckpoint(Some(s), c) }

    Observable.merge(microblockSink, blockSink, checkpointSink).subscribe()(monix.execution.Scheduler.Implicits.global)
    miner.scheduleMining()

    for (addr <- settings.networkSettings.declaredAddress if settings.networkSettings.uPnPSettings.enable) {
      upnp.addPort(addr.getPort)
    }

    implicit val as: ActorSystem = actorSystem
    implicit val materializer: ActorMaterializer = ActorMaterializer()

    if (settings.restAPISettings.enable) {
      val apiRoutes = Seq(
        BlocksApiRoute(settings.restAPISettings, history, blockchainUpdater, allChannels, c => processCheckpoint(None, c)),
        TransactionsApiRoute(settings.restAPISettings, stateReader, history, utxStorage),
        NxtConsensusApiRoute(settings.restAPISettings, stateReader, history, settings.blockchainSettings.functionalitySettings),
        WalletApiRoute(settings.restAPISettings, wallet),
        PaymentApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time),
        UtilsApiRoute(settings.restAPISettings),
        PeersApiRoute(settings.restAPISettings, network.connect, peerDatabase, establishedConnections),
        AddressApiRoute(settings.restAPISettings, wallet, stateReader, settings.blockchainSettings.functionalitySettings),
        DebugApiRoute(settings.restAPISettings, wallet, stateReader, history, peerDatabase, establishedConnections, blockchainUpdater, allChannels, utxStorage, blockchainDebugInfo, miner, configRoot),
        WavesApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time),
        AssetsApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, stateReader, time),
        NodeApiRoute(settings.restAPISettings, () => this.shutdown()),
        ActivationApiRoute(settings.restAPISettings, settings.blockchainSettings.functionalitySettings, settings.featuresSettings, history, featureProvider),
        AssetsBroadcastApiRoute(settings.restAPISettings, utxStorage, allChannels),
        LeaseApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time),
        LeaseBroadcastApiRoute(settings.restAPISettings, utxStorage, allChannels),
        AliasApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time, stateReader),
        AliasBroadcastApiRoute(settings.restAPISettings, utxStorage, allChannels)
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
        typeOf[ActivationApiRoute],
        typeOf[AssetsBroadcastApiRoute],
        typeOf[LeaseApiRoute],
        typeOf[LeaseBroadcastApiRoute],
        typeOf[AliasApiRoute],
        typeOf[AliasBroadcastApiRoute]
      )
      val combinedRoute: Route = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings.restAPISettings).compositeRoute
      val httpFuture = Http().bindAndHandle(combinedRoute, settings.restAPISettings.bindAddress, settings.restAPISettings.port)
      serverBinding = Await.result(httpFuture, 10.seconds)
      log.info(s"REST API was bound on ${settings.restAPISettings.bindAddress}:${settings.restAPISettings.port}")
    }

    //on unexpected shutdown
    sys.addShutdownHook {
      utxStorage.close()
      network.shutdown()
      shutdown()
    }

    if (settings.matcherSettings.enable) {
      val matcher = new Matcher(actorSystem, wallet, utxStorage, allChannels, stateReader, history,
        settings.blockchainSettings, settings.restAPISettings, settings.matcherSettings, db)
      matcher.runMatcher()
    }
  }

  @volatile var shutdownInProgress = false
  @volatile var serverBinding: ServerBinding = _

  def shutdown(): Unit = {
    val unbindTimeout = 2.minutes
    val stopActorsTimeout = 1.minute

    if (!shutdownInProgress) {
      log.info("Stopping network services")
      shutdownInProgress = true
      if (settings.restAPISettings.enable) {
        Try(Await.ready(serverBinding.unbind(), unbindTimeout))
          .failed.map(e => log.error("Failed to unbind REST API port", e))
      }
      for (addr <- settings.networkSettings.declaredAddress if settings.networkSettings.uPnPSettings.enable) {
        upnp.deletePort(addr.getPort)
      }

      peerDatabase.close()

      Try(Await.result(actorSystem.terminate(), stopActorsTimeout))
        .failed.map(e => log.error("Failed to terminate actor system", e))

      log.debug("Closing storage")
      db.close()

      log.info("Shutdown complete")
    }
  }

}

object Application extends ScorexLogging {

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
          forceStopApplication()
        }
        loadConfig(cfg)
    }

    config
  }

  def main(args: Array[String]): Unit = {
    // prevents java from caching successful name resolutions, which is needed e.g. for proper NTP server rotation
    // http://stackoverflow.com/a/17219327
    System.setProperty("sun.net.inetaddr.ttl", "0")
    System.setProperty("sun.net.inetaddr.negative.ttl", "0")
    Security.setProperty("networkaddress.cache.ttl", "0")
    Security.setProperty("networkaddress.cache.negative.ttl", "0")

    // j.u.l should log messages using the projects' conventions
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()

    val config = readConfig(args.headOption)

    // DO NOT LOG BEFORE THIS LINE, THIS PROPERTY IS USED IN logback.xml
    System.setProperty("waves.directory", config.getString("waves.directory"))
    log.info("Starting...")
    sys.addShutdownHook {
      SystemInformationReporter.report(config)
    }

    val settings = WavesSettings.fromConfig(config)
    Kamon.start(config)
    val isMetricsStarted = Metrics.start(settings.metrics)

==== BASE ====
    log.trace(s"System property sun.net.inetaddr.ttl=${System.getProperty("sun.net.inetaddr.ttl")}")
    log.trace(s"System property sun.net.inetaddr.negative.ttl=${System.getProperty("sun.net.inetaddr.negative.ttl")}")
    log.trace(s"Security property networkaddress.cache.ttl=${Security.getProperty("networkaddress.cache.ttl")}")
    log.trace(s"Security property networkaddress.cache.negative.ttl=${Security.getProperty("networkaddress.cache.negative.ttl")}")

    RootActorSystem.start("wavesplatform", config) { actorSystem =>
      import actorSystem.dispatcher
      isMetricsStarted.foreach { started =>
        if (started) {
          import settings.synchronizationSettings.microBlockSynchronizer
          import settings.{minerSettings => miner}

          Metrics.write(
            Point
              .measurement("config")
              .addField("miner-micro-block-interval", miner.microBlockInterval.toMillis)
              .addField("miner-max-transactions-in-key-block", miner.maxTransactionsInKeyBlock)
              .addField("miner-max-transactions-in-micro-block", miner.maxTransactionsInMicroBlock)
              .addField("miner-min-micro-block-age", miner.minMicroBlockAge.toMillis)
              .addField("mbs-wait-response-timeout", microBlockSynchronizer.waitResponseTimeout.toMillis)
          )
        }
      }

      // Initialize global var with actual address scheme
      AddressScheme.current = new AddressScheme {
        override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
      }

      log.info(s"${Constants.AgentName} Blockchain Id: ${settings.blockchainSettings.addressSchemeCharacter}")

      new Application(actorSystem, settings, config.root()) {
        override def shutdown(): Unit = {
          Kamon.shutdown()
          Metrics.shutdown()
          super.shutdown()
        }
      }.run()
    }
  }
}
