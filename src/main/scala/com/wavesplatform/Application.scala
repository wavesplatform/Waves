package com.wavesplatform

import java.io.File
import java.security.Security
import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.stream.ActorMaterializer
import cats.instances.all._
import com.typesafe.config._
import com.wavesplatform.actor.RootActorSystem
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.db.openDB
import com.wavesplatform.features.api.ActivationApiRoute
import com.wavesplatform.history.{CheckpointServiceImpl, StorageFactory}
import com.wavesplatform.http.NodeApiRoute
import com.wavesplatform.matcher.Matcher
import com.wavesplatform.metrics.Metrics
import com.wavesplatform.mining.{Miner, MinerImpl}
import com.wavesplatform.network.RxExtensionLoader.RxExtensionLoaderShutdownHook
import com.wavesplatform.network._
import com.wavesplatform.settings._
import com.wavesplatform.state.appender.{BlockAppender, CheckpointAppender, ExtensionAppender, MicroblockAppender}
import com.wavesplatform.utils.{SystemInformationReporter, forceStopApplication}
import com.wavesplatform.utx.{MatcherUtxPool, UtxPool, UtxPoolImpl}
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import kamon.Kamon
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler._
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
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

  import monix.execution.Scheduler.Implicits.{global => scheduler}

  private val db = openDB(settings.dataDirectory, settings.levelDbCacheSize)

  private val LocalScoreBroadcastDebounce = 1.second

  private val blockchainUpdater = StorageFactory(settings, db, NTP)

  private val checkpointService = new CheckpointServiceImpl(db, settings.checkpointsSettings)
  private lazy val upnp         = new UPnP(settings.networkSettings.uPnPSettings) // don't initialize unless enabled

  private val wallet: Wallet = try {
    Wallet(settings.walletSettings)
  } catch {
    case e: IllegalStateException =>
      log.error(s"Failed to open wallet file '${settings.walletSettings.file.get.getAbsolutePath}")
      throw e
  }
  private val peerDatabase = new PeerDatabaseImpl(settings.networkSettings)

  private val extensionLoaderScheduler        = singleThread("rx-extension-loader", reporter = log.error("Error in Extension Loader", _))
  private val microblockSynchronizerScheduler = singleThread("microblock-synchronizer", reporter = log.error("Error in Microblock Synchronizer", _))
  private val scoreObserverScheduler          = singleThread("rx-score-observer", reporter = log.error("Error in Score Observer", _))
  private val appenderScheduler               = singleThread("appender", reporter = log.error("Error in Appender", _))
  private val historyRepliesScheduler         = fixedPool("history-replier", poolSize = 2, reporter = log.error("Error in History Replier", _))
  private val minerScheduler                  = fixedPool("miner-pool", poolSize = 2, reporter = log.error("Error in Miner", _))

  private var matcher: Option[Matcher]                                         = None
  private var rxExtensionLoaderShutdown: Option[RxExtensionLoaderShutdownHook] = None
  private var maybeUtx: Option[UtxPool]                                        = None
  private var maybeNetwork: Option[NS]                                         = None

  def apiShutdown(): Unit = {
    for {
      u <- maybeUtx
      n <- maybeNetwork
    } yield shutdown(u, n)
  }

  def run(): Unit = {
    checkGenesis(settings, blockchainUpdater)

    if (wallet.privateKeyAccounts.isEmpty)
      wallet.generateNewAccounts(1)

    val feeCalculator          = new FeeCalculator(settings.feesSettings, blockchainUpdater)
    val time: Time             = NTP
    val establishedConnections = new ConcurrentHashMap[Channel, PeerInfo]
    val allChannels            = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    val innerUtxStorage =
      new UtxPoolImpl(time, blockchainUpdater, feeCalculator, settings.blockchainSettings.functionalitySettings, settings.utxSettings)

    matcher = if (settings.matcherSettings.enable) {
      val m = new Matcher(actorSystem,
                          wallet,
                          innerUtxStorage,
                          allChannels,
                          blockchainUpdater,
                          settings.blockchainSettings,
                          settings.restAPISettings,
                          settings.matcherSettings)
      m.runMatcher()
      Some(m)
    } else None

    val utxStorage =
      if (settings.matcherSettings.enable) new MatcherUtxPool(innerUtxStorage, settings.matcherSettings, actorSystem.eventStream) else innerUtxStorage
    maybeUtx = Some(utxStorage)

    val knownInvalidBlocks = new InvalidBlockStorageImpl(settings.synchronizationSettings.invalidBlocksStorage)

    val pos = new PoSSelector(blockchainUpdater, settings.blockchainSettings)

    val miner =
      if (settings.minerSettings.enable)
        new MinerImpl(allChannels, blockchainUpdater, checkpointService, settings, time, utxStorage, wallet, pos, minerScheduler, appenderScheduler)
      else Miner.Disabled

    val processBlock =
      BlockAppender(checkpointService, blockchainUpdater, time, utxStorage, pos, settings, allChannels, peerDatabase, miner, appenderScheduler) _

    val processCheckpoint =
      CheckpointAppender(checkpointService, blockchainUpdater, blockchainUpdater, peerDatabase, miner, allChannels, appenderScheduler) _

    val processFork = ExtensionAppender(
      checkpointService,
      blockchainUpdater,
      utxStorage,
      pos,
      time,
      settings,
      knownInvalidBlocks,
      peerDatabase,
      miner,
      allChannels,
      appenderScheduler
    ) _
    val processMicroBlock =
      MicroblockAppender(checkpointService, blockchainUpdater, utxStorage, allChannels, peerDatabase, appenderScheduler) _

    import blockchainUpdater.lastBlockInfo

    val lastScore = lastBlockInfo
      .map(_.score)
      .distinctUntilChanged
      .share(scheduler)

    lastScore
      .debounce(LocalScoreBroadcastDebounce)
      .foreach { x =>
        allChannels.broadcast(LocalScoreChanged(x))
      }(scheduler)

    val historyReplier = new HistoryReplier(blockchainUpdater, settings.synchronizationSettings, historyRepliesScheduler)
    val network =
      NetworkServer(settings, lastBlockInfo, blockchainUpdater, historyReplier, utxStorage, peerDatabase, allChannels, establishedConnections)
    maybeNetwork = Some(network)
    val (signatures, blocks, blockchainScores, checkpoints, microblockInvs, microblockResponses, transactions) = network.messages

    val timeoutSubject: ConcurrentSubject[Channel, Channel] = ConcurrentSubject.publish[Channel]

    val (syncWithChannelClosed, scoreStatsReporter) = RxScoreObserver(
      settings.synchronizationSettings.scoreTTL,
      1.second,
      blockchainUpdater.score,
      lastScore,
      blockchainScores,
      network.closedChannels,
      timeoutSubject,
      scoreObserverScheduler
    )
    val (microblockDatas, mbSyncCacheSizes) = MicroBlockSynchronizer(
      settings.synchronizationSettings.microBlockSynchronizer,
      peerDatabase,
      lastBlockInfo.map(_.id),
      microblockInvs,
      microblockResponses,
      microblockSynchronizerScheduler
    )
    val (newBlocks, extLoaderState, sh) = RxExtensionLoader(
      settings.synchronizationSettings.synchronizationTimeout,
      Coeval(blockchainUpdater.lastBlockIds(settings.synchronizationSettings.maxRollback)),
      peerDatabase,
      knownInvalidBlocks,
      blocks,
      signatures,
      syncWithChannelClosed,
      extensionLoaderScheduler,
      timeoutSubject
    ) { case ((c, b)) => processFork(c, b.blocks) }

    rxExtensionLoaderShutdown = Some(sh)

    UtxPoolSynchronizer.start(utxStorage, settings.synchronizationSettings.utxSynchronizerSettings, allChannels, transactions)
    val microBlockSink = microblockDatas.mapTask(scala.Function.tupled(processMicroBlock))
    val blockSink      = newBlocks.mapTask(scala.Function.tupled(processBlock))
    val checkpointSink = checkpoints.mapTask { case ((s, c)) => processCheckpoint(Some(s), c) }

    Observable.merge(microBlockSink, blockSink, checkpointSink).subscribe()
    miner.scheduleMining()

    for (addr <- settings.networkSettings.declaredAddress if settings.networkSettings.uPnPSettings.enable) {
      upnp.addPort(addr.getPort)
    }

    implicit val as: ActorSystem                 = actorSystem
    implicit val materializer: ActorMaterializer = ActorMaterializer()

    if (settings.restAPISettings.enable) {
      val apiRoutes = Seq(
        NodeApiRoute(settings.restAPISettings, blockchainUpdater, () => apiShutdown()),
        BlocksApiRoute(settings.restAPISettings, blockchainUpdater, blockchainUpdater, allChannels, c => processCheckpoint(None, c)),
        TransactionsApiRoute(settings.restAPISettings, wallet, blockchainUpdater, utxStorage, allChannels, time),
        NxtConsensusApiRoute(settings.restAPISettings, blockchainUpdater, settings.blockchainSettings.functionalitySettings),
        WalletApiRoute(settings.restAPISettings, wallet),
        PaymentApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time),
        UtilsApiRoute(time, settings.restAPISettings),
        PeersApiRoute(settings.restAPISettings, network.connect, peerDatabase, establishedConnections),
        AddressApiRoute(settings.restAPISettings,
                        wallet,
                        blockchainUpdater,
                        utxStorage,
                        allChannels,
                        time,
                        settings.blockchainSettings.functionalitySettings),
        DebugApiRoute(
          settings,
          wallet,
          blockchainUpdater,
          peerDatabase,
          establishedConnections,
          blockId => Task(blockchainUpdater.removeAfter(blockId)).executeOn(appenderScheduler),
          allChannels,
          utxStorage,
          miner,
          historyReplier,
          extLoaderState,
          mbSyncCacheSizes,
          scoreStatsReporter,
          configRoot
        ),
        WavesApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time),
        AssetsApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, blockchainUpdater, time),
        ActivationApiRoute(settings.restAPISettings, settings.blockchainSettings.functionalitySettings, settings.featuresSettings, blockchainUpdater),
        AssetsBroadcastApiRoute(settings.restAPISettings, utxStorage, allChannels),
        LeaseApiRoute(settings.restAPISettings, wallet, blockchainUpdater, utxStorage, allChannels, time),
        LeaseBroadcastApiRoute(settings.restAPISettings, utxStorage, allChannels),
        AliasApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time, blockchainUpdater),
        AliasBroadcastApiRoute(settings.restAPISettings, utxStorage, allChannels)
      )

      val apiTypes = Seq(
        typeOf[NodeApiRoute],
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
        typeOf[ActivationApiRoute],
        typeOf[AssetsBroadcastApiRoute],
        typeOf[LeaseApiRoute],
        typeOf[LeaseBroadcastApiRoute],
        typeOf[AliasApiRoute],
        typeOf[AliasBroadcastApiRoute]
      )
      val combinedRoute = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings.restAPISettings).loggingCompositeRoute
      val httpFuture    = Http().bindAndHandle(combinedRoute, settings.restAPISettings.bindAddress, settings.restAPISettings.port)
      serverBinding = Await.result(httpFuture, 20.seconds)
      log.info(s"REST API was bound on ${settings.restAPISettings.bindAddress}:${settings.restAPISettings.port}")
    }

    //on unexpected shutdown
    sys.addShutdownHook {
      Kamon.shutdown()
      Metrics.shutdown()
      shutdown(utxStorage, network)
    }
  }

  @volatile var shutdownInProgress           = false
  @volatile var serverBinding: ServerBinding = _

  def shutdown(utx: UtxPool, network: NS): Unit = {
    if (!shutdownInProgress) {
      shutdownInProgress = true

      utx.close()

      shutdownAndWait(historyRepliesScheduler, "HistoryReplier", 5.minutes)

      log.info("Closing REST API")
      if (settings.restAPISettings.enable) {
        Try(Await.ready(serverBinding.unbind(), 2.minutes)).failed.map(e => log.error("Failed to unbind REST API port", e))
      }
      for (addr <- settings.networkSettings.declaredAddress if settings.networkSettings.uPnPSettings.enable) {
        upnp.deletePort(addr.getPort)
      }

      matcher.foreach(_.shutdownMatcher())

      log.debug("Closing peer database")
      peerDatabase.close()

      Try(Await.result(actorSystem.terminate(), 2.minute)).failed.map(e => log.error("Failed to terminate actor system", e))

      blockchainUpdater.shutdown()
      rxExtensionLoaderShutdown.foreach(_.shutdown())

      log.info("Stopping network services")
      network.shutdown()

      shutdownAndWait(minerScheduler, "Miner")
      shutdownAndWait(microblockSynchronizerScheduler, "MicroblockSynchronizer")
      shutdownAndWait(scoreObserverScheduler, "ScoreObserver")
      shutdownAndWait(extensionLoaderScheduler, "ExtensionLoader")
      shutdownAndWait(appenderScheduler, "Appender", 5.minutes)

      log.info("Closing storage")
      db.close()

      log.info("Shutdown complete")
    }
  }

  private def shutdownAndWait(scheduler: SchedulerService, name: String, timeout: FiniteDuration = 1.minute): Unit = {
    scheduler.shutdown()
    val r = Await.result(scheduler.awaitTermination(timeout, global), Duration.Inf)
    if (r)
      log.info(s"$name was shutdown successfully")
    else
      log.warn(s"Failed to shutdown $name properly during timeout")
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

    // specify aspectj to use it's build-in infrastructure
    // http://www.eclipse.org/aspectj/doc/released/pdguide/trace.html
    System.setProperty("org.aspectj.tracing.factory", "default")

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

      new Application(actorSystem, settings, config.root()).run()
    }
  }
}
