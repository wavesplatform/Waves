package com.wavesplatform.riderunner.app

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import com.google.common.io.{MoreFiles, RecursiveDeleteOption}
import com.wavesplatform.api.http.CompositeHttpService
import com.wavesplatform.api.{DefaultBlockchainApi, GrpcChannelSettings, GrpcConnector}
import com.wavesplatform.blockchain.{BlockchainProcessor, BlockchainState}
import com.wavesplatform.database.RDB
import com.wavesplatform.http.{EvaluateApiRoute, HttpServiceStatus, ServiceApiRoute}
import com.wavesplatform.jvm.HeapDumps
import com.wavesplatform.riderunner.DefaultRequestsService
import com.wavesplatform.riderunner.storage.persistent.LevelDbPersistentCaches
import com.wavesplatform.riderunner.storage.{LevelDbRequestsStorage, RequestKey, SharedBlockchainStorage, Storage}
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging
import io.grpc.ManagedChannel
import io.netty.util.concurrent.DefaultThreadFactory
import kamon.instrumentation.executor.ExecutorInstrumentation
import monix.eval.Task
import monix.execution.{ExecutionModel, Scheduler}
import play.api.libs.json.Json
import sttp.client3.HttpURLConnectionBackend

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.*
import java.util.stream.Collector
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}
import scala.io.Source
import scala.util.Using

object RideWithBlockchainUpdatesService extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val (globalConfig, settings) = AppInitializer.init(args.headOption.map(new File(_)))

    log.info("Starting...")
    implicit val actorSystem = ActorSystem("ride-runner", globalConfig)
    val cs                   = new Cleanup(actorSystem)

    val metricsEnabled = globalConfig.getBoolean("kamon.enable")
    if (metricsEnabled) {
      val metrics = new RideRunnerMetrics(globalConfig)
      cs.cleanup(CustomShutdownPhase.Metrics) { metrics.close() }
    }

    HeapDumps.cleanDirs(5)

    log.info("Initializing thread pools...")
    def mkScheduler(name: String, threads: Int): Scheduler = {
      val executor = new ThreadPoolExecutor(
        threads,
        threads,
        0,
        TimeUnit.MILLISECONDS,
        new LinkedBlockingQueue[Runnable],
        new DefaultThreadFactory(name, true),
        { (r: Runnable, executor: ThreadPoolExecutor) =>
          log.error(s"$r has been rejected from $executor")
          throw new RejectedExecutionException
        }
      )

      val monixScheduler = Scheduler(
        executor = if (metricsEnabled) ExecutorInstrumentation.instrument(executor, name) else executor,
        executionModel = ExecutionModel.AlwaysAsyncExecution
      )

      cs.cleanupTask(CustomShutdownPhase.ThreadPools, name) {
        monixScheduler.shutdown()
        monixScheduler.awaitTermination(5.seconds)

        executor.shutdown()
        try executor.awaitTermination(5, TimeUnit.SECONDS)
        finally executor.shutdownNow()
      }

      monixScheduler
    }

    val blockchainEventsStreamScheduler = mkScheduler("blockchain-events", 2)
    val rideScheduler                   = mkScheduler(name = "ride", threads = settings.rideRunner.exactRideSchedulerThreads)

    val grpcConnector = new GrpcConnector(settings.rideRunner.grpcConnector)
    cs.cleanup(CustomShutdownPhase.GrpcConnector) { grpcConnector.close() }

    def mkGrpcChannel(name: String, settings: GrpcChannelSettings): ManagedChannel = {
      val grpcApiChannel = grpcConnector.mkChannel(settings)
      cs.cleanupTask(CustomShutdownPhase.ApiClient, name) {
        grpcApiChannel.shutdown()
        try grpcApiChannel.awaitTermination(5, TimeUnit.SECONDS)
        finally grpcApiChannel.shutdownNow()
      }
      grpcApiChannel
    }

    log.info("Making gRPC channel to gRPC API...")
    val grpcApiChannel = mkGrpcChannel("grpcApi", settings.rideRunner.grpcApiChannel)

    log.info("Making gRPC channel to Blockchain Updates API...")
    val blockchainUpdatesApiChannel = mkGrpcChannel("blockchainUpdatesApi", settings.rideRunner.blockchainUpdatesApiChannel)

    log.info("Creating general API gateway...")
    val httpBackend = HttpURLConnectionBackend()
    cs.cleanupTask(CustomShutdownPhase.ApiClient, "httpBackend") { httpBackend.close() }

    val blockchainApi = new DefaultBlockchainApi(
      settings = settings.rideRunner.blockchainApi,
      grpcApiChannel = grpcApiChannel,
      blockchainUpdatesApiChannel = blockchainUpdatesApiChannel,
      httpBackend = httpBackend
    )

    // TODO HACK: remove
    {
      val cleanupIterationPath = Paths.get(settings.rideRunner.db.directory, "..", "cleanup").normalize()
      val cleanupIteration =
        if (cleanupIterationPath.toFile.exists()) Files.readString(cleanupIterationPath, StandardCharsets.UTF_8).toIntOption.getOrElse(0)
        else 0

      val cleanTo = 1
      if (cleanupIteration < cleanTo) {
        log.info(s"Cleaning the DB with caches from $cleanupIteration to $cleanTo...")
        MoreFiles.deleteRecursively(Paths.get(settings.rideRunner.db.directory))
        Files.writeString(cleanupIterationPath, cleanTo.toString)
      }
    }

    log.info("Opening a DB with caches...")
    val db = RDB.open(settings.rideRunner.db.toNode).db
    cs.cleanup(CustomShutdownPhase.Db) { db.close() }

    val storage = Storage.rocksDb(db)
    val blockchainStorage = storage.readWrite { implicit rw =>
      val dbCaches = LevelDbPersistentCaches(storage)
      SharedBlockchainStorage[RequestKey](settings.rideRunner.sharedBlockchain, storage, dbCaches, blockchainApi)
    }

    val lastHeightAtStart = Height(blockchainApi.getCurrentBlockchainHeight())
    log.info(s"Current height: known=${blockchainStorage.height}, blockchain=$lastHeightAtStart")

    val requestsStorage = new LevelDbRequestsStorage(storage)
    log.info(s"There are ${requestsStorage.all().size} scripts")

    val requestsService = new DefaultRequestsService(
      settings.rideRunner.requestsService,
      storage,
      blockchainStorage,
      requestsStorage,
      rideScheduler
    )

    log.info("Warming up caches...") // Helps to figure out, which data is used by a script
    Await.result(requestsService.runAll().runToFuture(rideScheduler), Duration.Inf)

    val lastSafeKnownHeight = Height(math.max(0, blockchainStorage.height - 100 - 1)) // A rollback is not possible
    val workingHeight       = Height(math.max(blockchainStorage.height, lastHeightAtStart))

    log.info(s"Watching blockchain updates...")
    val blockchainUpdatesStream = blockchainApi.mkBlockchainUpdatesStream(blockchainEventsStreamScheduler)
    cs.cleanup(CustomShutdownPhase.BlockchainUpdatesStream) { blockchainUpdatesStream.close() }

    val processor = new BlockchainProcessor(blockchainStorage, requestsService)

    @volatile var lastServiceStatus = ServiceStatus()
    val events = blockchainUpdatesStream.downstream
      .doOnError(e => Task { log.error("Error!", e) })
      .scanEval(Task.now[BlockchainState](BlockchainState.Starting(lastSafeKnownHeight, workingHeight))) {
        BlockchainState(processor, blockchainUpdatesStream, _, _)
      }
      .doOnNext { state =>
        Task {
          lastServiceStatus = ServiceStatus(
            maxObservedHeight = state.processedHeight,
            lastProcessedHeight = math.max(lastServiceStatus.lastProcessedHeight, state.processedHeight),
            lastProcessedTimeMs = blockchainEventsStreamScheduler.clockMonotonic(TimeUnit.MILLISECONDS),
            healthy = state match {
              case _: BlockchainState.Working => true
              case _                          => false
            }
          )
        }
      }
      .doOnError { e =>
        Task {
          log.error("Got an unhandled error, closing streams. Contact with developers", e)
          blockchainUpdatesStream.close()
        }
      }
      .lastL
      .runToFuture(blockchainEventsStreamScheduler)

    blockchainUpdatesStream.start(lastSafeKnownHeight + 1)

    log.info(s"Initializing REST API on ${settings.restApi.bindAddress}:${settings.restApi.port}...")
    val apiRoutes = Seq(
      EvaluateApiRoute(requestsService.trackAndRun(_).runToFuture(rideScheduler)),
      ServiceApiRoute(
        settings.rideRunner.serviceApiRoute,
        { () =>
          val nowMs      = blockchainEventsStreamScheduler.clockMonotonic(TimeUnit.MILLISECONDS)
          val idleTimeMs = nowMs - lastServiceStatus.lastProcessedTimeMs
          HttpServiceStatus(
            healthy = lastServiceStatus.healthy && idleTimeMs < settings.rideRunner.unhealthyIdleTimeoutMs,
            debug = Json.obj(
              "nowTime"             -> nowMs,
              "lastProcessedTime"   -> lastServiceStatus.lastProcessedTimeMs,
              "lastProcessedHeight" -> lastServiceStatus.lastProcessedHeight,
              "idleTime"            -> idleTimeMs,
              "maxObservedHeight"   -> lastServiceStatus.maxObservedHeight
            )
          )
        }
      )
    )

    val httpService = CompositeHttpService(apiRoutes, settings.restApi)
    val httpFuture = Http()
      .newServerAt(settings.restApi.bindAddress, settings.restApi.port)
      .bindFlow(httpService.loggingCompositeRoute)
    Await.result(httpFuture, 20.seconds)

    log.info("Initialization completed")
    Await.result(events, Duration.Inf)

    log.info("Done")
    cs.forceStop()
  }

  private case class ServiceStatus(
      healthy: Boolean = false,
      nowTimeMs: Long = 0,
      lastProcessedTimeMs: Long = 0,
      lastProcessedHeight: Int = 0,
      idleTimeMs: Long = 0,
      maxObservedHeight: Int = 0
  )
}
