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
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.actor.RootActorSystem
import com.wavesplatform.api.http._
import com.wavesplatform.api.http.alias.{AliasApiRoute, AliasBroadcastApiRoute}
import com.wavesplatform.api.http.assets.{AssetsApiRoute, AssetsBroadcastApiRoute}
import com.wavesplatform.api.http.leasing.{LeaseApiRoute, LeaseBroadcastApiRoute}
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.consensus.nxt.api.http.NxtConsensusApiRoute
import com.wavesplatform.db.openDB
import com.wavesplatform.features.api.ActivationApiRoute
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.http.{DebugApiRoute, NodeApiRoute, WavesApiRoute}
import com.wavesplatform.matcher.Matcher
import com.wavesplatform.metrics.Metrics
import com.wavesplatform.mining.{Miner, MinerImpl}
import com.wavesplatform.network.RxExtensionLoader.RxExtensionLoaderShutdownHook
import com.wavesplatform.network._
import com.wavesplatform.settings._
import com.wavesplatform.state.appender.{BlockAppender, ExtensionAppender, MicroblockAppender}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.{NTP, ScorexLogging, SystemInformationReporter}
import com.wavesplatform.utx.{UtxPool, UtxPoolImpl}
import com.wavesplatform.wallet.Wallet
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import kamon.Kamon
import kamon.influxdb.CustomInfluxDBReporter
import kamon.system.SystemMetrics
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler._
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import org.influxdb.dto.Point
import org.slf4j.bridge.SLF4JBridgeHandler

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal

class Application(val actorSystem: ActorSystem, val settings: WavesSettings, configRoot: ConfigObject, time: NTP) extends ScorexLogging {

  import monix.execution.Scheduler.Implicits.{global => scheduler}

  private val db = openDB(settings.dataDirectory)

  private val LocalScoreBroadcastDebounce = 1.second

  private val spendableBalanceChanged = ConcurrentSubject.publish[(Address, Asset)]

  private val blockchainUpdater = StorageFactory(settings, db, time, spendableBalanceChanged)

  private lazy val upnp = new UPnP(settings.networkSettings.uPnPSettings) // don't initialize unless enabled

  private val wallet: Wallet = try {
    Wallet(settings.walletSettings)
  } catch {
    case NonFatal(e) =>
      log.error(s"Failed to open wallet file '${settings.walletSettings.file.get.getAbsolutePath}", e)
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

    val establishedConnections = new ConcurrentHashMap[Channel, PeerInfo]
    val allChannels            = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    val utxStorage =
      new UtxPoolImpl(time, blockchainUpdater, spendableBalanceChanged, settings.blockchainSettings.functionalitySettings, settings.utxSettings)
    maybeUtx = Some(utxStorage)

    matcher = if (settings.matcherSettings.enable) {
      Matcher(actorSystem, time, wallet, utxStorage, allChannels, blockchainUpdater, spendableBalanceChanged, settings)
    } else None

    val knownInvalidBlocks = new InvalidBlockStorageImpl(settings.synchronizationSettings.invalidBlocksStorage)

    val pos = new PoSSelector(blockchainUpdater, settings.blockchainSettings, settings.synchronizationSettings)

    val miner =
      if (settings.minerSettings.enable)
        new MinerImpl(allChannels, blockchainUpdater, settings, time, utxStorage, wallet, pos, minerScheduler, appenderScheduler)
      else Miner.Disabled

    val processBlock =
      BlockAppender(blockchainUpdater, time, utxStorage, pos, settings, allChannels, peerDatabase, miner, appenderScheduler) _

    val processFork =
      ExtensionAppender(blockchainUpdater, utxStorage, pos, time, settings, knownInvalidBlocks, peerDatabase, miner, allChannels, appenderScheduler) _
    val processMicroBlock =
      MicroblockAppender(blockchainUpdater, utxStorage, allChannels, peerDatabase, appenderScheduler) _

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
    val (signatures, blocks, blockchainScores, microblockInvs, microblockResponses, transactions) = network.messages

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
    ) { case (c, b) => processFork(c, b.blocks) }

    rxExtensionLoaderShutdown = Some(sh)

    UtxPoolSynchronizer.start(utxStorage, settings.synchronizationSettings.utxSynchronizerSettings, allChannels, transactions)

    val microBlockSink = microblockDatas
      .mapTask(scala.Function.tupled(processMicroBlock))

    val blockSink = newBlocks
      .mapTask(scala.Function.tupled(processBlock))

    Observable.merge(microBlockSink, blockSink).subscribe()
    miner.scheduleMining()
    utxStorage.cleanup.runCleanupOn(blockSink)

    for (addr <- settings.networkSettings.declaredAddress if settings.networkSettings.uPnPSettings.enable) {
      upnp.addPort(addr.getPort)
    }

    implicit val as: ActorSystem                 = actorSystem
    implicit val materializer: ActorMaterializer = ActorMaterializer()

    if (settings.restAPISettings.enable) {
      val apiRoutes = Seq(
        NodeApiRoute(settings.restAPISettings, blockchainUpdater, () => apiShutdown()),
        BlocksApiRoute(settings.restAPISettings, blockchainUpdater, allChannels),
        TransactionsApiRoute(settings.restAPISettings,
                             settings.blockchainSettings.functionalitySettings,
                             wallet,
                             blockchainUpdater,
                             utxStorage,
                             allChannels,
                             time),
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
          time,
          blockchainUpdater,
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

      val apiTypes: Set[Class[_]] = Set(
        classOf[NodeApiRoute],
        classOf[BlocksApiRoute],
        classOf[TransactionsApiRoute],
        classOf[NxtConsensusApiRoute],
        classOf[WalletApiRoute],
        classOf[UtilsApiRoute],
        classOf[PeersApiRoute],
        classOf[AddressApiRoute],
        classOf[DebugApiRoute],
        classOf[AssetsApiRoute],
        classOf[ActivationApiRoute],
        classOf[LeaseApiRoute],
        classOf[AliasApiRoute]
      )
      val combinedRoute = CompositeHttpService(apiTypes, apiRoutes, settings.restAPISettings)(actorSystem).loggingCompositeRoute
      val httpFuture    = Http().bindAndHandle(combinedRoute, settings.restAPISettings.bindAddress, settings.restAPISettings.port)
      serverBinding = Await.result(httpFuture, 20.seconds)
      log.info(s"REST API was bound on ${settings.restAPISettings.bindAddress}:${settings.restAPISettings.port}")
    }

    //on unexpected shutdown
    sys.addShutdownHook {
      Await.ready(Kamon.stopAllReporters(), 20.seconds)
      Metrics.shutdown()
      shutdown(utxStorage, network)
    }
  }

  @volatile var shutdownInProgress           = false
  @volatile var serverBinding: ServerBinding = _

  def shutdown(utx: UtxPool, network: NS): Unit = {
    if (!shutdownInProgress) {
      shutdownInProgress = true

      spendableBalanceChanged.onComplete()
      utx.close()

      shutdownAndWait(historyRepliesScheduler, "HistoryReplier", 5.minutes)

      log.info("Closing REST API")
      if (settings.restAPISettings.enable) {
        Try(Await.ready(serverBinding.unbind(), 2.minutes)).failed.map(e => log.error("Failed to unbind REST API port", e))
      }
      for (addr <- settings.networkSettings.declaredAddress if settings.networkSettings.uPnPSettings.enable) {
        upnp.deletePort(addr.getPort)
      }

      matcher.foreach(_.shutdown())

      log.debug("Closing peer database")
      peerDatabase.close()

      Try(Await.result(actorSystem.terminate(), 2.minute)).failed.map(e => log.error("Failed to terminate actor system", e))
      log.debug("Node's actor system shutdown successful")

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

      time.close()
      log.info("Shutdown complete")
    }
  }

  private def shutdownAndWait(scheduler: SchedulerService, name: String, timeout: FiniteDuration = 1.minute): Unit = {
    log.debug(s"Shutting down $name")
    scheduler.shutdown()
    val r = Await.result(scheduler.awaitTermination(timeout, global), 2 * timeout)
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
    } yield ConfigFactory.parseFile(file)

    loadConfig(maybeConfigFile)
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

    // Initialize global var with actual address scheme
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    if (config.getBoolean("kamon.enable")) {
      log.info("Aggregated metrics are enabled")
      Kamon.reconfigure(config)
      Kamon.addReporter(new CustomInfluxDBReporter())
      SystemMetrics.startCollecting()
    }

    val time             = new NTP(settings.ntpServer)
    val isMetricsStarted = Metrics.start(settings.metrics, time)

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

      log.info(s"${Constants.AgentName} Blockchain Id: ${settings.blockchainSettings.addressSchemeCharacter}")

      new Application(actorSystem, settings, config.root(), time).run()
    }
  }
}
