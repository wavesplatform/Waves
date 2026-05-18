package com.wavesplatform

import cats.Eq
import cats.instances.bigInt.*
import cats.syntax.option.*
import com.typesafe.config.*
import com.typesafe.scalalogging.Logger
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.actor.RootActorSystem
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.*
import com.wavesplatform.api.http.*
import com.wavesplatform.api.http.alias.AliasApiRoute
import com.wavesplatform.api.http.assets.AssetsApiRoute
import com.wavesplatform.api.http.eth.EthRpcRoute
import com.wavesplatform.api.http.leasing.LeaseApiRoute
import com.wavesplatform.api.http.utils.UtilsApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.database.{DBExt, Keys, RDB}
import com.wavesplatform.events.{BlockchainUpdateTriggers, UtxEvent}
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.features.EstimatorProvider.*
import com.wavesplatform.features.api.ActivationApiRoute
import com.wavesplatform.history.{History, StorageFactory}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.Metrics
import com.wavesplatform.mining.{BlockChallengerImpl, Miner, MinerDebugInfo, MinerImpl}
import com.wavesplatform.network.*
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.appender.{BlockAppender, ExtensionAppender, MicroblockAppender}
import com.wavesplatform.state.{BlockEndorser, BlockRewardCalculator, Blockchain, CompleteBlockchainUpdater, EndorsementStorage, Height, TxMeta}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{DiscardedBlocks, Transaction}
import com.wavesplatform.utils.*
import com.wavesplatform.utils.Schedulers.*
import com.wavesplatform.utx.{UtxPool, UtxPoolImpl}
import com.wavesplatform.wallet.Wallet
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.HashedWheelTimer
import io.netty.util.concurrent.{DefaultThreadFactory, GlobalEventExecutor}
import kamon.Kamon
import kamon.instrumentation.executor.ExecutorInstrumentation
import monix.eval.{Coeval, Task}
import monix.execution.schedulers.{ExecutorScheduler, SchedulerService}
import monix.execution.{ExecutionModel, Scheduler, UncaughtExceptionReporter}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.Http.ServerBinding
import org.influxdb.dto.Point
import org.rocksdb.RocksDB
import org.slf4j.LoggerFactory

import java.io.File
import java.security.Security
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{TimeUnit, *}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

class Application(val actorSystem: ActorSystem, val settings: WavesSettings, configRoot: ConfigObject, time: NTP) extends ScorexLogging {
  app =>

  import Application.*
  import monix.execution.Scheduler.Implicits.global as scheduler

  private val rdb = RDB.open(settings.dbSettings)

  private lazy val upnp = new UPnP(settings.networkSettings.uPnPSettings) // don't initialize unless enabled

  private val wallet: Wallet = Wallet(settings.walletSettings)

  private val peerDatabase = new PeerDatabaseImpl(settings.networkSettings)

  // This handler is needed in case Fatal exception is thrown inside the task

  private val stopOnAppendError = UncaughtExceptionReporter { cause =>
    log.error("Error in Appender", cause)
    forceStopApplication(FatalDBError)
  }

  private val appenderScheduler = singleThread("appender", stopOnAppendError)

  private val extensionLoaderScheduler = singleThread("rx-extension-loader", reporter = log.error("Error in Extension Loader", _))
  private val microblockSynchronizerScheduler =
    singleThread("microblock-synchronizer", reporter = log.error("Error in Microblock Synchronizer", _))
  private val endorseBlockSynchronizerScheduler =
    singleThread("endorseblock-synchronizer", reporter = log.error("Error in EndorseBlock Synchronizer", _))
  private val scoreObserverScheduler  = singleThread("rx-score-observer", reporter = log.error("Error in Score Observer", _))
  private val historyRepliesScheduler = fixedPool(poolSize = 2, "history-replier", reporter = log.error("Error in History Replier", _))
  private val minerScheduler          = singleThread("block-miner", reporter = log.error("Error in Miner", _))

  private val utxEvents = ConcurrentSubject.publish[UtxEvent](using scheduler)

  private var extensions = Seq.empty[Extension]

  private var triggers = Seq.empty[BlockchainUpdateTriggers]

  private var miner: Miner & MinerDebugInfo = Miner.StrictDisabledMiner
  private val (blockchainUpdater, rocksDB) =
    StorageFactory(settings, rdb, time, BlockchainUpdateTriggers.combined(triggers), Miner.forwardTo(miner))

  private val messageObserver = new MessageObserver

  @volatile
  private var maybeUtx: Option[UtxPool] = None

  @volatile
  private var maybeNetworkServer: Option[NetworkServer] = None

  @volatile
  private var serverBinding: ServerBinding = compiletime.uninitialized

  def run(): Unit = {
    // initialization
    implicit val as: ActorSystem = actorSystem

    if (wallet.privateKeyAccounts.isEmpty)
      wallet.generateNewAccounts(1)

    val establishedConnections = new ConcurrentHashMap[Channel, PeerInfo]
    val allChannels            = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    val utxStorage =
      new UtxPoolImpl(time, blockchainUpdater, settings.utxSettings, settings.maxTxErrorLogSize, settings.minerSettings.enable, utxEvents.onNext)
    maybeUtx = Some(utxStorage)

    val timer                 = new HashedWheelTimer()
    val utxSynchronizerLogger = Logger(LoggerFactory.getLogger(classOf[TransactionPublisher]))
    val timedTxValidator =
      Schedulers.timeBoundedFixedPool(
        timer,
        5.seconds,
        settings.synchronizationSettings.utxSynchronizer.maxThreads,
        "utx-time-bounded-tx-validator",
        reporter = utxSynchronizerLogger.trace("Uncaught exception in UTX Synchronizer", _)
      )

    val knownInvalidBlocks = new InvalidBlockStorageImpl(settings.synchronizationSettings.invalidBlocksStorage)

    val pos = PoSSelector(blockchainUpdater, settings.synchronizationSettings.maxBaseTarget)

    val endorsementStorage = EndorsementStorage.InMemory((blockId, height) => blockchainUpdater.blockId(height.toInt).contains(blockId))
    val blockEndorser =
      new BlockEndorser.InMemory(settings.synchronizationSettings.maxRollback, blockchainUpdater, wallet, endorsementStorage, allChannels)

    if (settings.minerSettings.enable)
      miner = new MinerImpl(
        allChannels,
        blockchainUpdater,
        settings,
        time,
        utxStorage,
        blockEndorser,
        endorsementStorage,
        wallet,
        pos,
        minerScheduler,
        appenderScheduler,
        utxEvents.collect { case _: UtxEvent.TxAdded =>
          ()
        }
      )

    val blockChallenger =
      if (settings.minerSettings.enable && !settings.enableLightMode) {
        Some(
          new BlockChallengerImpl(
            blockchainUpdater,
            allChannels,
            wallet,
            settings,
            time,
            pos,
            // BlockEndorser is disabled, because the challenging block doesn't contain finalization voting header
            appendBlock = BlockAppender(blockchainUpdater, time, utxStorage, pos, BlockEndorser.Disabled, appenderScheduler)(_, None)
          )
        )
      } else None

    val processBlock =
      BlockAppender(blockchainUpdater, time, utxStorage, pos, allChannels, peerDatabase, blockChallenger, blockEndorser, appenderScheduler)

    val processFork =
      ExtensionAppender(blockchainUpdater, utxStorage, pos, time, knownInvalidBlocks, peerDatabase, appenderScheduler)
    val processMicroBlock =
      MicroblockAppender(blockchainUpdater, utxStorage, allChannels, peerDatabase, blockChallenger, appenderScheduler)

    import blockchainUpdater.lastBlockInfo

    val lastScore = lastBlockInfo
      .map(_.score)
      .distinctUntilChanged
      .share(using scheduler)

    lastScore
      .debounce(1.second)
      .foreach { x =>
        allChannels.broadcast(LocalScoreChanged(x))
      }(using scheduler)

    val history = History(
      blockchainUpdater,
      blockchainUpdater.liquidBlock,
      blockchainUpdater.microBlock,
      blockchainUpdater.liquidBlockSnapshot,
      blockchainUpdater.microBlockSnapshot,
      rdb
    )

    val historyReplier = new HistoryReplier(blockchainUpdater.score, history, settings.synchronizationSettings)(using historyRepliesScheduler)

    val transactionPublisher =
      TransactionPublisher.timeBounded(
        utxStorage.putIfNew,
        allChannels.broadcast,
        timedTxValidator,
        settings.synchronizationSettings.utxSynchronizer.allowTxRebroadcasting,
        () =>
          if (allChannels.size >= settings.restAPISettings.minimumPeers) Right(())
          else Left(GenericError(s"There are not enough connections with peers (${allChannels.size}) to accept transaction"))
      )

    def rollbackTask(blockId: ByteStr, returnTxsToUtx: Boolean) =
      Task {
        utxStorage.resetPriorityPool()
        blockchainUpdater.removeAfter(blockId)
      }.executeOn(appenderScheduler)
        .map {
          case Right(discardedBlocks) =>
            allChannels.broadcast(LocalScoreChanged(blockchainUpdater.score))
            if (returnTxsToUtx) utxStorage.addAndScheduleCleanup(discardedBlocks.view.flatMap(_._1.transactionData))
            Right(discardedBlocks)
          case Left(error) => Left(error)
        }

    // Extensions start
    val extensionContext: Context = new Context {
      override def settings: WavesSettings                                                      = app.settings
      override def blockchain: Blockchain                                                       = app.blockchainUpdater
      override def rollbackTo(blockId: ByteStr): Task[Either[ValidationError, DiscardedBlocks]] = rollbackTask(blockId, returnTxsToUtx = false)
      override def time: Time                                                                   = app.time
      override def wallet: Wallet                                                               = app.wallet
      override def utx: UtxPool                                                                 = utxStorage
      override def broadcastTransaction(tx: Transaction): TracedResult[ValidationError, Boolean] =
        Await.result(transactionPublisher.validateAndBroadcast(tx, None), Duration.Inf) // TODO: Replace with async if possible
      override def utxEvents: Observable[UtxEvent] = app.utxEvents

      override val transactionsApi: CommonTransactionsApi = CommonTransactionsApi(
        blockchainUpdater.bestLiquidSnapshot.map(Height(blockchainUpdater.height) -> _),
        rdb,
        blockchainUpdater,
        utxStorage,
        blockChallenger,
        tx => transactionPublisher.validateAndBroadcast(tx, None),
        loadBlockAt(rdb, blockchainUpdater)
      )
      override val blocksApi: CommonBlocksApi = CommonBlocksApi(
        settings.synchronizationSettings.maxRollback,
        blockchainUpdater,
        loadBlockMetaAt(rdb.db, blockchainUpdater),
        loadBlockInfoAt(rdb, blockchainUpdater)
      )
      override val accountsApi: CommonAccountsApi =
        CommonAccountsApi(() => blockchainUpdater.snapshotBlockchain, rdb, blockchainUpdater)
      override val assetsApi: CommonAssetsApi =
        CommonAssetsApi(() => blockchainUpdater.bestLiquidSnapshot.orEmpty, rdb.db, blockchainUpdater)
      override def generatorsApi: CommonGeneratorsApi =
        CommonGeneratorsApi(rdb, blockchainUpdater)
    }

    extensions = settings.extensions.map { extensionClassName =>
      val extensionClass = Class.forName(extensionClassName).asInstanceOf[Class[Extension]]
      val ctor           = extensionClass.getConstructor(classOf[Context])
      log.info(s"Enable extension: $extensionClassName")
      ctor.newInstance(extensionContext)
    }
    triggers ++= extensions.collect { case e: BlockchainUpdateTriggers => e }
    extensions.foreach(_.start())

    // Node start
    // After this point, node actually starts doing something
    appenderScheduler.execute(() => checkGenesis(settings, blockchainUpdater, miner))

    // Network server should be started only after all extensions initialized
    val networkServer =
      NetworkServerL1(
        settings,
        lastBlockInfo,
        historyReplier,
        peerDatabase,
        messageObserver,
        allChannels,
        establishedConnections
      )
    maybeNetworkServer = Some(networkServer)
    val timeoutSubject: ConcurrentSubject[Channel, Channel] = ConcurrentSubject.publish[Channel]

    val (syncWithChannelClosed, scoreStatsReporter) = RxScoreObserver(
      settings.synchronizationSettings.scoreTTL,
      1.second,
      blockchainUpdater.score,
      lastScore,
      messageObserver.blockchainScores,
      networkServer.closedChannels,
      timeoutSubject,
      scoreObserverScheduler
    )
    val (microblockDataWithSnapshot, mbSyncCacheSizes) = MicroBlockSynchronizer(
      settings.synchronizationSettings.microBlockSynchronizer,
      settings.enableLightMode,
      peerDatabase,
      lastBlockInfo.map(_.id),
      messageObserver.microblockInvs,
      messageObserver.microblockResponses,
      messageObserver.microblockSnapshots,
      microblockSynchronizerScheduler
    )

    messageObserver.endorseBlocks.foreach { case (ch, x) =>
      endorsementStorage.tryAdd(x) match {
        case Left(err)   => log.trace(s"Unexpected $x: $err")
        case Right(true) => allChannels.broadcast(x, Some(ch))
        case _           =>
      }
    }(using endorseBlockSynchronizerScheduler)

    val (newBlocksWithSnapshot, extLoaderState, _) = RxExtensionLoader(
      settings.synchronizationSettings.synchronizationTimeout,
      settings.synchronizationSettings.processedBlocksCacheTimeout,
      settings.enableLightMode,
      settings.synchronizationSettings.blacklistOnScoreMismatch,
      Coeval(blockchainUpdater.lastBlockIds(settings.synchronizationSettings.maxRollback)),
      peerDatabase,
      knownInvalidBlocks,
      messageObserver.blocks,
      messageObserver.signatures,
      messageObserver.blockSnapshots,
      syncWithChannelClosed,
      extensionLoaderScheduler,
      timeoutSubject
    ) { case (c, b) =>
      processFork(c, b).doOnFinish {
        case None    => Task.now(())
        case Some(e) => Task(stopOnAppendError.reportFailure(e))
      }
    }

    TransactionSynchronizer(
      settings.synchronizationSettings.utxSynchronizer,
      lastBlockInfo.map(_.id).distinctUntilChanged(using Eq.fromUniversalEquals),
      messageObserver.transactions,
      transactionPublisher
    )

    Observable(
      microblockDataWithSnapshot
        .mapEval(processMicroBlock.tupled),
      newBlocksWithSnapshot
        .mapEval(processBlock.tupled)
    ).mergeMap(identity)
      .onErrorHandle(stopOnAppendError.reportFailure)
      .subscribe()

    // API start
    if (settings.restAPISettings.enable) {

      val limitedScheduler =
        Schedulers.timeBoundedFixedPool(
          new HashedWheelTimer(),
          5.seconds,
          settings.restAPISettings.limitedPoolThreads,
          "rest-time-limited",
          reporter = log.trace("Uncaught exception in time limited pool", _)
        )
      val heavyRequestProcessorPoolThreads =
        settings.restAPISettings.heavyRequestProcessorPoolThreads.getOrElse((Runtime.getRuntime.availableProcessors() * 2).min(4))
      val heavyRequestExecutor = new ThreadPoolExecutor(
        heavyRequestProcessorPoolThreads,
        heavyRequestProcessorPoolThreads,
        0,
        TimeUnit.MILLISECONDS,
        new LinkedBlockingQueue[Runnable],
        new DefaultThreadFactory("rest-heavy-request-processor", true),
        { (r: Runnable, executor: ThreadPoolExecutor) =>
          log.error(s"$r has been rejected from $executor")
          throw new RejectedExecutionException
        }
      )

      val heavyRequestScheduler = Scheduler(
        if (settings.config.getBoolean("kamon.enable"))
          ExecutorInstrumentation.instrument(heavyRequestExecutor, "heavy-request-executor")
        else heavyRequestExecutor,
        ExecutionModel.BatchedExecution(100)
      )

      val serverRequestTimeout = FiniteDuration(settings.config.getDuration("pekko.http.server.request-timeout").getSeconds, TimeUnit.SECONDS)
      val routeTimeout         = new RouteTimeout(serverRequestTimeout)(using heavyRequestScheduler)

      val apiRoutes = Seq(
        new EthRpcRoute(blockchainUpdater, extensionContext.transactionsApi, time),
        NodeApiRoute(settings.restAPISettings, blockchainUpdater, () => shutdown()),
        BlocksApiRoute(settings.restAPISettings, extensionContext.blocksApi, time, routeTimeout),
        TransactionsApiRoute(
          settings.restAPISettings,
          extensionContext.transactionsApi,
          wallet,
          blockchainUpdater,
          () => blockchainUpdater.snapshotBlockchain,
          () => utxStorage.size,
          transactionPublisher,
          time,
          routeTimeout
        ),
        WalletApiRoute(settings.restAPISettings, wallet),
        UtilsApiRoute(
          time,
          settings.restAPISettings,
          settings.maxTxErrorLogSize,
          () => blockchainUpdater.estimator,
          limitedScheduler,
          blockchainUpdater
        ),
        PeersApiRoute(settings.restAPISettings, address => networkServer.connect(address), peerDatabase, establishedConnections),
        AddressApiRoute(
          settings.restAPISettings,
          wallet,
          blockchainUpdater,
          transactionPublisher,
          time,
          limitedScheduler,
          routeTimeout,
          extensionContext.accountsApi,
          settings.dbSettings.maxRollbackDepth
        ),
        GeneratorsApiRoute(settings.restAPISettings, blockchainUpdater, extensionContext.generatorsApi, time, routeTimeout),
        DebugApiRoute(
          settings,
          time,
          blockchainUpdater,
          wallet,
          extensionContext.accountsApi,
          extensionContext.transactionsApi,
          extensionContext.assetsApi,
          peerDatabase,
          establishedConnections,
          (id, returnTxs) => rollbackTask(id, returnTxs).map(_.map(_ => ())),
          utxStorage,
          miner,
          historyReplier,
          extLoaderState,
          mbSyncCacheSizes,
          scoreStatsReporter,
          configRoot,
          rocksDB,
          routeTimeout,
          heavyRequestScheduler
        ),
        AssetsApiRoute(
          settings.restAPISettings,
          serverRequestTimeout,
          wallet,
          blockchainUpdater,
          () => blockchainUpdater.snapshotBlockchain,
          time,
          extensionContext.accountsApi,
          extensionContext.assetsApi,
          settings.dbSettings.maxRollbackDepth,
          routeTimeout
        ),
        ActivationApiRoute(settings.restAPISettings, settings.featuresSettings, blockchainUpdater),
        LeaseApiRoute(
          settings.restAPISettings,
          wallet,
          blockchainUpdater,
          transactionPublisher,
          time,
          extensionContext.accountsApi,
          routeTimeout
        ),
        AliasApiRoute(
          settings.restAPISettings,
          extensionContext.transactionsApi,
          wallet,
          transactionPublisher,
          time,
          blockchainUpdater,
          routeTimeout
        ),
        RewardApiRoute(blockchainUpdater),
        FinalityApiRoute(blockchainUpdater, extensionContext.blocksApi, extensionContext.generatorsApi)
      )

      val httpService = CompositeHttpService(apiRoutes, settings.restAPISettings)
      val httpFuture =
        Http().newServerAt(settings.restAPISettings.bindAddress, settings.restAPISettings.port).bindFlow(httpService.loggingCompositeRoute)
      serverBinding = Await.result(httpFuture, 20.seconds)
      serverBinding.whenTerminated.foreach(_ => heavyRequestScheduler.shutdown())
      log.info(s"REST API was bound on ${settings.restAPISettings.bindAddress}:${settings.restAPISettings.port}")
    }

    for (addr <- settings.networkSettings.derivedDeclaredAddress if settings.networkSettings.uPnPSettings.enable) {
      upnp.addPort(addr.getPort)
    }

    // on unexpected shutdown
    sys.addShutdownHook {
      timer.stop()
      shutdown()
    }
  }

  private val shutdownInProgress = new AtomicBoolean(false)

  def shutdown(): Unit =
    if (shutdownInProgress.compareAndSet(false, true)) {
      maybeUtx.foreach(_.close())

      log.info("Closing REST API")
      if (settings.restAPISettings.enable)
        Try(Await.ready(serverBinding.unbind(), 2.minutes)).failed.map(e => log.error("Failed to unbind REST API port", e))
      for (addr <- settings.networkSettings.derivedDeclaredAddress if settings.networkSettings.uPnPSettings.enable) upnp.deletePort(addr.getPort)

      log.debug("Closing peer database")
      peerDatabase.close()

      Try(Await.result(actorSystem.terminate(), 2.minute)).failed.map(e => log.error("Failed to terminate actor system", e))
      log.debug("Node's actor system shutdown successful")

      blockchainUpdater.shutdown()

      maybeNetworkServer.foreach { network =>
        log.info("Stopping network services")
        network.shutdown()
      }
      messageObserver.shutdown()

      shutdownAndWait(appenderScheduler, "Appender", 5.minutes.some)

      log.info("Closing storage")
      rocksDB.close()
      rdb.close()

      // extensions should be shut down last, after all node functionality, to guarantee no data loss
      if (extensions.nonEmpty) {
        log.info(s"Shutting down extensions")
        Await.ready(Future.sequence(extensions.map(_.shutdown())), settings.extensionsShutdownTimeout)
      }

      time.close()
      log.info("Shutdown complete")
    }

  private def shutdownAndWait(scheduler: SchedulerService, name: String, timeout: Option[FiniteDuration], tryForce: Boolean = true): Unit = {
    log.debug(s"Shutting down $name")
    scheduler match {
      case es: ExecutorScheduler if tryForce => es.executor.shutdownNow()
      case s                                 => s.shutdown()
    }
    timeout.foreach { to =>
      val r = Await.result(scheduler.awaitTermination(to, global), 2 * to)
      if (r)
        log.info(s"$name was shutdown successfully")
      else
        log.warn(s"Failed to shutdown $name properly during timeout")
    }
  }
}

object Application extends ScorexLogging {
  def loadApplicationConfig(external: Option[File] = None): WavesSettings = {
    import com.wavesplatform.settings.*

    val maybeExternalConfig = Try(external.map(f => ConfigFactory.parseFile(f.getAbsoluteFile, ConfigParseOptions.defaults().setAllowMissing(false))))
    val config              = loadConfig(maybeExternalConfig.getOrElse(None))

    // DO NOT LOG BEFORE THIS LINE, THIS PROPERTY IS USED IN logback.xml
    System.setProperty("waves.directory", config.getString("waves.directory"))
    if (config.hasPath("waves.config.directory")) System.setProperty("waves.config.directory", config.getString("waves.config.directory"))

    maybeExternalConfig match {
      case Success(None) =>
        val currentBlockchainType = Try(ConfigFactory.defaultOverrides().getString("waves.blockchain.type"))
          .orElse(Try(ConfigFactory.defaultOverrides().getString("waves.defaults.blockchain.type")))
          .map(_.toUpperCase)
          .getOrElse("TESTNET")

        log.info(s"Config file not specified, default $currentBlockchainType config will be used")
      case Failure(exception) =>
        log.error(s"Couldn't read ${external.get.toPath.toAbsolutePath}", exception)
        forceStopApplication(Misconfiguration)
      case _ => // Pass
    }

    val settings = WavesSettings.fromRootConfig(config)

    // Initialize global var with actual address scheme
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    // IMPORTANT: to make use of default settings for histograms and timers, it's crucial to reconfigure Kamon with
    //            our merged config BEFORE initializing any metrics, including in settings-related companion objects
    if (config.getBoolean("kamon.enable")) {
      Kamon.init(config)
    } else {
      Kamon.reconfigure(config)
    }

    sys.addShutdownHook {
      Try(Await.result(Kamon.stop(), 30 seconds))
      Metrics.shutdown()
    }

    val DisabledHash = "H6nsiifwYKYEx6YzYD7woP1XCn72RVvx6tC1zjjLXqsu"
    if (settings.restAPISettings.enable && settings.restAPISettings.apiKeyHash == DisabledHash) {
      log.error(s"Usage of the default api key hash ($DisabledHash) is prohibited, please change it in the waves.conf")
      forceStopApplication(Misconfiguration)
    }

    settings
  }

  private[wavesplatform] def loadBlockAt(rdb: RDB, blockchainUpdater: CompleteBlockchainUpdater)(
      height: Height
  ): Option[(BlockMeta, Seq[(TxMeta, Transaction)])] =
    loadBlockInfoAt(rdb, blockchainUpdater)(height)

  private[wavesplatform] def loadBlockInfoAt(rdb: RDB, blockchainUpdater: CompleteBlockchainUpdater)(
      height: Height
  ): Option[(BlockMeta, Seq[(TxMeta, Transaction)])] =
    loadBlockMetaAt(rdb.db, blockchainUpdater)(height).map { meta =>
      meta -> blockchainUpdater
        .liquidTransactions(meta.id)
        .getOrElse(database.loadTransactions(height, rdb))
    }

  private[wavesplatform] def loadBlockMetaAt(db: RocksDB, blockchainUpdater: CompleteBlockchainUpdater)(height: Height): Option[BlockMeta] =
    blockchainUpdater.liquidBlockMeta
      .filter(_ => blockchainUpdater.height == height.toInt)
      .orElse(db.get(Keys.blockMetaAt(height)).flatMap(BlockMeta.fromPb))
      .map { blockMeta =>
        val rewardShares = BlockRewardCalculator.getSortedBlockRewardShares(height.toInt, blockMeta.header.generator.toAddress, blockchainUpdater)
        blockMeta.copy(
          rewardShares = rewardShares,
          reward = blockMeta.reward.map(_ * blockchainUpdater.blockRewardBoost(height))
        )
      }

  def main(args: Array[String]): Unit = {

    // prevents java from caching successful name resolutions, which is needed e.g. for proper NTP server rotation
    // http://stackoverflow.com/a/17219327
    System.setProperty("sun.net.inetaddr.ttl", "0")
    System.setProperty("sun.net.inetaddr.negative.ttl", "0")
    Security.setProperty("networkaddress.cache.ttl", "0")
    Security.setProperty("networkaddress.cache.negative.ttl", "0")

    args.headOption.getOrElse("") match {
      case "export"                 => Exporter.main(args.tail)
      case "import"                 => Importer.main(args.tail)
      case "explore"                => Explorer.main(args.tail)
      case "util"                   => UtilApp.main(args.tail)
      case "gengen"                 => GenesisBlockGenerator.main(args.tail)
      case "help" | "--help" | "-h" => println("Usage: waves <config> | export | import | explore | util | gengen")
      case _                        => startNode(args.headOption)
    }
  }

  private def startNode(configFile: Option[String]): Unit = {
    import com.wavesplatform.settings.Constants
    val settings = loadApplicationConfig(configFile.map(new File(_)))

    val log      = Logger(LoggerFactory.getLogger(getClass))
    val modeInfo = if (settings.enableLightMode) "in light mode" else "in full mode"
    log.info(s"Starting $modeInfo...")
    sys.addShutdownHook {
      SystemInformationReporter.report(settings.config)
    }

    val time = new NTP(settings.ntpServer)
    Metrics.start(settings.metrics, time)

    def dumpMinerConfig(): Unit = {
      import settings.minerSettings as miner
      import settings.synchronizationSettings.microBlockSynchronizer

      Metrics.write(
        Point
          .measurement("config")
          .addField("miner-micro-block-interval", miner.microBlockInterval.toMillis)
          .addField("miner-max-transactions-in-micro-block", miner.maxTransactionsInMicroBlock)
          .addField("miner-min-micro-block-age", miner.minMicroBlockAge.toMillis)
          .addField("mbs-wait-response-timeout", microBlockSynchronizer.waitResponseTimeout.toMillis)
      )
    }

    RootActorSystem.start("wavesplatform", settings.config) { actorSystem =>
      dumpMinerConfig()
      log.info(s"${Constants.AgentName} Blockchain Id: ${settings.blockchainSettings.addressSchemeCharacter}")
      new Application(actorSystem, settings, settings.config.root(), time).run()
    }
  }
}
