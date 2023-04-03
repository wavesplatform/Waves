package com.wavesplatform.ride.runner.entrypoints

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import com.google.common.io.MoreFiles
import com.wavesplatform.api.http.CompositeHttpService
import com.wavesplatform.api.{DefaultBlockchainApi, GrpcChannelSettings, GrpcConnector}
import com.wavesplatform.ride.runner.db.RideRocksDb
import com.wavesplatform.ride.runner.http.{EvaluateApiRoute, HttpServiceStatus, ServiceApiRoute}
import com.wavesplatform.ride.runner.requests.{DefaultRequestService, SynchronizedJobScheduler}
import com.wavesplatform.ride.runner.stats.RideRunnerStats
import com.wavesplatform.ride.runner.storage.persistent.DefaultPersistentCaches
import com.wavesplatform.ride.runner.storage.{ScriptRequest, SharedBlockchainStorage}
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
import sttp.client3.HttpURLConnectionBackend

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.*
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}

object RideRunnerWithBlockchainUpdatesService extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val (globalConfig, settings) = AppInitializer.init(args.headOption.map(new File(_)))

    log.info("Starting...")
    implicit val actorSystem = ActorSystem("ride-runner", globalConfig)
    val cs                   = new Cleanup(actorSystem)

    val metrics = new RideRunnerStats(globalConfig)
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

    // TODO HACK: Remove when the storage format cemented
    {
      val rootPath             = Paths.get(settings.rideRunner.db.directory, "..").normalize()
      val cleanupIterationPath = rootPath.resolve("cleanup")
      val cleanupIteration =
        if (cleanupIterationPath.toFile.exists()) Files.readString(cleanupIterationPath, StandardCharsets.UTF_8).trim
        else "-1" // to differ cases

      if (cleanupIteration == "-1") {
        rootPath.toFile.listFiles().foreach { file =>
          println(s"File before: $file")
        }
      }

      val cleanTo = 1 // Increase if you want to clean the database
      if (cleanupIteration.toIntOption.getOrElse(-2) < cleanTo) {
        log.info(
          s"Cleaning the DB with caches in ${settings.rideRunner.db.directory} from $cleanupIteration ($cleanupIterationPath) to $cleanTo..."
        )
        rootPath.toFile.listFiles().foreach { file =>
          if (file.getAbsolutePath != cleanupIterationPath.toAbsolutePath.toString) MoreFiles.deleteRecursively(file.toPath)
        }
        Files.writeString(cleanupIterationPath, cleanTo.toString)
      }

      if (cleanupIteration == "-1") {
        rootPath.toFile.listFiles().foreach { file =>
          println(s"File after: $file")
        }
      }
    }

    log.info("Opening a DB with caches...")
    val rideDb          = RideRocksDb.open(settings.rideRunner.db)
    val sendDbStatsTask = blockchainEventsStreamScheduler.scheduleAtFixedRate(30.seconds, 30.seconds) { rideDb.sendStats() }
    cs.cleanup(CustomShutdownPhase.Db) {
      sendDbStatsTask.cancel()
      rideDb.close()
    }

    log.info("Loading data from caches...")
    val sharedBlockchain = rideDb.access.readWrite { implicit rw =>
      val dbCaches = DefaultPersistentCaches(rideDb.access)
      SharedBlockchainStorage[ScriptRequest](settings.rideRunner.sharedBlockchain, rideDb.access, dbCaches, blockchainApi)
    }

    val lastHeightAtStart = Height(blockchainApi.getCurrentBlockchainHeight())
    log.info(s"Current height: shared (local or network)=${sharedBlockchain.height}, network=$lastHeightAtStart")

    val requestService = new DefaultRequestService(
      settings.rideRunner.requestsService,
      rideDb.access,
      sharedBlockchain,
      SynchronizedJobScheduler(50),
      rideScheduler
    )

    val lastSafeKnownHeight = Height(math.max(1, sharedBlockchain.height - 100 - 1)) // A rollback is not possible
    val workingHeight       = Height(math.max(sharedBlockchain.height, lastHeightAtStart))

    log.info(s"Watching blockchain updates...")
    val blockchainUpdatesStream = blockchainApi.mkBlockchainUpdatesStream(blockchainEventsStreamScheduler)
    cs.cleanup(CustomShutdownPhase.BlockchainUpdatesStream) { blockchainUpdatesStream.close() }

    val processor = new BlockchainProcessor(sharedBlockchain, requestService)

    @volatile var lastServiceStatus = ServiceStatus()
    val events = blockchainUpdatesStream.downstream
      .doOnError(e => Task { log.error("Error!", e) })
      .scanEval(Task.now[BlockchainState](BlockchainState.Starting(lastSafeKnownHeight, workingHeight))) {
        BlockchainState(settings.rideRunner.blockchainState, processor, blockchainUpdatesStream, _, _)
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
      EvaluateApiRoute(requestService.trackAndRun(_).runToFuture(rideScheduler)),
      ServiceApiRoute(
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
    val http = Await.result(httpFuture, 20.seconds)
    cs.cleanup(CustomShutdownPhase.BlockchainUpdatesStream) { http.terminate(5.seconds) }

//    val gc = blockchainEventsStreamScheduler.scheduleAtFixedRate(20.minutes, 20.minutes) {
//      log.info("Running GC...")
//      System.gc()
//    }
//    cs.cleanup(CustomShutdownPhase.BlockchainUpdatesStream) { gc.cancel() }

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
