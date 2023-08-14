package com.wavesplatform.ride.runner.entrypoints

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import com.wavesplatform.api.http.CompositeHttpService
import com.wavesplatform.api.{DefaultBlockchainApi, GrpcChannelSettings, GrpcConnector}
import com.wavesplatform.ride.runner.caches.persistent.DefaultPersistentCaches
import com.wavesplatform.ride.runner.caches.{CacheKeyTags, SharedBlockchainStorage}
import com.wavesplatform.ride.runner.db.RideRocksDb
import com.wavesplatform.ride.runner.http.{EvaluateApiRoute, HttpServiceStatus, ServiceApiRoute}
import com.wavesplatform.ride.runner.requests.{DefaultRequestService, RideScriptRunRequest, SynchronizedJobScheduler}
import com.wavesplatform.ride.runner.stats.RideRunnerStats
import com.wavesplatform.ride.runner.{BlockchainProcessor, BlockchainState}
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging
import io.grpc.ManagedChannel
import io.netty.util.concurrent.DefaultThreadFactory
import kamon.Kamon
import kamon.instrumentation.executor.ExecutorInstrumentation
import monix.eval.Task
import monix.execution.{ExecutionModel, Scheduler}
import play.api.libs.json.Json

import java.io.File
import java.util.concurrent.*
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}

object WavesRideRunnerWithBlockchainService extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val (globalConfig, settings) = AppInitializer.init(externalConfig = args.headOption.map(new File(_).getAbsoluteFile))

    log.info("Starting...")
    // It has to be before other code: https://github.com/kamon-io/Kamon/issues/601#issuecomment-748995094
    val metrics = new RideRunnerStats(globalConfig)

    implicit val actorSystem = ActorSystem("ride-runner", globalConfig)
    val cs                   = new Cleanup(actorSystem)
    cs.cleanup(CustomShutdownPhase.Metrics) { metrics.close() }

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
        executor = if (Kamon.enabled()) ExecutorInstrumentation.instrument(executor, name) else executor,
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

    val grpcConnector = new GrpcConnector(settings.rideRunner.grpcConnectorExecutorThreads)
    cs.cleanup(CustomShutdownPhase.GrpcConnector) { grpcConnector.close() }

    def mkGrpcChannel(name: String, target: String, settings: GrpcChannelSettings): ManagedChannel = {
      val grpcApiChannel = grpcConnector.mkChannel(target, settings)
      cs.cleanupTask(CustomShutdownPhase.ApiClient, name) {
        grpcApiChannel.shutdown()
        try grpcApiChannel.awaitTermination(5, TimeUnit.SECONDS)
        finally grpcApiChannel.shutdownNow()
      }
      grpcApiChannel
    }

    log.info("Making gRPC channel to gRPC API...")
    val grpcApiChannel = mkGrpcChannel("grpcApi", settings.publicApi.grpcApi, settings.rideRunner.grpcApiChannel)

    log.info("Making gRPC channel to Blockchain Updates API...")
    val blockchainUpdatesApiChannel =
      mkGrpcChannel("blockchainUpdatesApi", settings.publicApi.grpcBlockchainUpdatesApi, settings.rideRunner.blockchainUpdatesApiChannel)

    log.info("Creating general API gateway...")
    val blockchainApi = new DefaultBlockchainApi(
      settings = settings.blockchainApi,
      grpcApiChannel = grpcApiChannel,
      blockchainUpdatesApiChannel = blockchainUpdatesApiChannel
    )

    val rideDb          = RideRocksDb.open(settings.rideRunner.db)
    val sendDbStatsTask = rideDb.startCollectingStats(blockchainEventsStreamScheduler)
    cs.cleanup(CustomShutdownPhase.Db) {
      sendDbStatsTask.cancel()
      rideDb.close()
    }

    val allTags = new CacheKeyTags[RideScriptRunRequest]
    val sharedBlockchain = rideDb.access.batchedReadOnly { implicit ro =>
      val dbCaches = DefaultPersistentCaches(rideDb.access)
      SharedBlockchainStorage(settings.sharedBlockchain, allTags, rideDb.access, dbCaches, blockchainApi)
    }

    val lastHeightAtStart = blockchainApi.getCurrentBlockchainHeight()
    val heights           = Heights.calculate(settings.heightsSettings, sharedBlockchain.heightUntaggedOpt, lastHeightAtStart)
    def localHeightStr    = sharedBlockchain.heightUntaggedOpt.fold("empty")(_.toString)
    log.info(s"Heights: local=$localHeightStr, local hardened=${heights.lastKnownHardened}, network=$lastHeightAtStart, working=${heights.working}")

    val requestService = new DefaultRequestService(
      settings.requestsService,
      sharedBlockchain,
      allTags,
      new SynchronizedJobScheduler()(rideScheduler),
      rideScheduler
    )
    cs.cleanup(CustomShutdownPhase.BlockchainUpdatesStream) { requestService.close() }

    log.info(s"Watching blockchain updates...")
    val blockchainUpdatesStream = blockchainApi.mkBlockchainUpdatesStream(blockchainEventsStreamScheduler)
    cs.cleanup(CustomShutdownPhase.BlockchainUpdatesStream) { blockchainUpdatesStream.close() }

    val processor = new BlockchainProcessor(sharedBlockchain, requestService)

    @volatile var lastServiceStatus = ServiceStatus()
    val events = blockchainUpdatesStream.downstream
      .doOnError(e => Task { log.error("Error!", e) })
      .scanEval(Task.now[BlockchainState](BlockchainState.Starting(heights.lastKnownHardened, heights.working))) {
        BlockchainState.applyWithRestarts(settings.blockchainState, processor, blockchainUpdatesStream, _, _)
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

    blockchainUpdatesStream.start(Height(heights.lastKnownHardened + 1))

    log.info(s"Initializing REST API on ${settings.restApi.bindAddress}:${settings.restApi.port}...")
    val apiRoutes = Seq(
      EvaluateApiRoute(requestService.trackAndRun(_).runToFuture(rideScheduler)),
      ServiceApiRoute(
        { () =>
          val nowMs      = blockchainEventsStreamScheduler.clockMonotonic(TimeUnit.MILLISECONDS)
          val idleTimeMs = nowMs - lastServiceStatus.lastProcessedTimeMs
          HttpServiceStatus(
            healthy = lastServiceStatus.healthy && idleTimeMs < settings.unhealthyIdleTimeoutMs,
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
    val http = Await.result(httpFuture, 20.seconds)
    cs.cleanup(CustomShutdownPhase.BlockchainUpdatesStream) { http.terminate(5.seconds) }

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
