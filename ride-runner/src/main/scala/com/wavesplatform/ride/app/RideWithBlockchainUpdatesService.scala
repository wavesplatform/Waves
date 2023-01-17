package com.wavesplatform.ride.app

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import com.wavesplatform.api.http.CompositeHttpService
import com.wavesplatform.blockchain.{BlockchainProcessor, BlockchainState, SharedBlockchainData}
import com.wavesplatform.database.openDB
import com.wavesplatform.api.{DefaultBlockchainApi, GrpcChannelSettings, GrpcConnector}
import com.wavesplatform.http.EvaluateApiRoute
import com.wavesplatform.state.Height
import com.wavesplatform.storage.LevelDbRequestsStorage
import com.wavesplatform.storage.RequestsStorage.RequestKey
import com.wavesplatform.storage.persistent.LevelDbPersistentCaches
import com.wavesplatform.utils.ScorexLogging
import io.grpc.ManagedChannel
import io.netty.util.concurrent.DefaultThreadFactory
import kamon.instrumentation.executor.ExecutorInstrumentation
import monix.eval.Task
import monix.execution.{ExecutionModel, Scheduler}
import sttp.client3.HttpURLConnectionBackend

import java.io.File
import java.util.concurrent.*
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}

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
    val rideScheduler = mkScheduler(
      name = "ride",
      threads = settings.restApi.heavyRequestProcessorPoolThreads.getOrElse((Runtime.getRuntime.availableProcessors() * 2).min(4))
    )

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

    log.info("Opening a caches DB...")
    val db = openDB(settings.rideRunner.db.directory)
    cs.cleanup(CustomShutdownPhase.Db) { db.close() }
    val dbCaches          = new LevelDbPersistentCaches(db)
    val blockchainStorage = new SharedBlockchainData[RequestKey](settings.blockchain, dbCaches, blockchainApi)

    val lastHeightAtStart = Height(blockchainApi.getCurrentBlockchainHeight())
    log.info(s"Current height: known=${blockchainStorage.height}, blockchain=$lastHeightAtStart")

    val requestsStorage = new LevelDbRequestsStorage(db)
    val processor = new BlockchainProcessor(
      settings.rideRunner.processor,
      blockchainStorage,
      requestsStorage,
      rideScheduler
    )

    log.info("Warming up caches...") // Helps to figure out, which data is used by a script
    Await.result(processor.runScripts(forceAll = true).runToFuture(rideScheduler), Duration.Inf)

    val lastSafeKnownHeight = Height(math.max(0, blockchainStorage.height - 100 - 1)) // A rollback is not possible
    val workingHeight       = Height(math.max(blockchainStorage.height, lastHeightAtStart))

    log.info(s"Watching blockchain updates...")
    val blockchainUpdatesStream = blockchainApi.mkBlockchainUpdatesStream(blockchainEventsStreamScheduler)
    cs.cleanup(CustomShutdownPhase.BlockchainUpdatesStream) { blockchainUpdatesStream.close() }

    val events = blockchainUpdatesStream.downstream
      .doOnError(e => Task { log.error("Error!", e) })
      .scanEval(Task.now[BlockchainState](BlockchainState.Starting(lastSafeKnownHeight, workingHeight))) {
        BlockchainState(processor, blockchainUpdatesStream, _, _)
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
      EvaluateApiRoute(Function.tupled(processor.getCachedResultOrRun(_, _).runToFuture(rideScheduler)))
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
}
