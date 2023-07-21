package com.wavesplatform.ride.runner.entrypoints

import akka.actor.ActorSystem
import com.wavesplatform.api.{DefaultBlockchainApi, GrpcChannelSettings, GrpcConnector}
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.ride.runner.db.RideRocksDb
import com.wavesplatform.ride.runner.requests.{DefaultRequestService, RideScriptRunRequest, SynchronizedJobScheduler}
import com.wavesplatform.ride.runner.stats.RideRunnerStats
import com.wavesplatform.ride.runner.storage.{CacheKeyTags, SharedBlockchainStorage}
import com.wavesplatform.ride.runner.storage.persistent.DefaultPersistentCaches
import com.wavesplatform.ride.runner.{BlockchainProcessor, BlockchainState}
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging
import io.grpc.ManagedChannel
import io.netty.util.concurrent.DefaultThreadFactory
import kamon.instrumentation.executor.ExecutorInstrumentation
import monix.eval.Task
import monix.execution.{ExecutionModel, Scheduler}
import play.api.libs.json.Json

import java.io.File
import java.util.concurrent.{LinkedBlockingQueue, RejectedExecutionException, ThreadPoolExecutor, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}
import scala.io.Source
import scala.util.Using

object RideRunnerWithBlockchainUpdatesApp extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val (globalConfig, settings) = AppInitializer.init(externalConfig = args.headOption.map(new File(_)))

    log.info("Loading args...")
    val inputFile =
      if (args.length < 2) throw new IllegalArgumentException("Please specify an input.json file")
      else new File(args(1))

    val scripts = Json
      .parse(Using(Source.fromFile(inputFile))(_.getLines().mkString("\n")).get)
      .as[List[RideScriptRunRequest]]

    log.info(s"Found ${scripts.size} scripts")
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
        executor = if (globalConfig.getBoolean("kamon.enable")) ExecutorInstrumentation.instrument(executor, name) else executor,
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
    cs.cleanup(CustomShutdownPhase.GrpcConnector) {
      grpcConnector.close()
    }

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
    val blockchainApi = new DefaultBlockchainApi(
      settings = settings.rideRunner.blockchainApi,
      grpcApiChannel = grpcApiChannel,
      blockchainUpdatesApiChannel = blockchainUpdatesApiChannel
    )

    log.info("Opening a caches DB...")
    val db = RideRocksDb.open(settings.rideRunner.db)
    cs.cleanup(CustomShutdownPhase.Db) { db.close() }

    val allTags = new CacheKeyTags[RideScriptRunRequest]
    val sharedBlockchain = db.access.batchedReadOnly { implicit rw =>
      val dbCaches = DefaultPersistentCaches(db.access)
      SharedBlockchainStorage(settings.rideRunner.sharedBlockchain, allTags, db.access, dbCaches, blockchainApi)
    }

    val localHeightAtStart          = sharedBlockchain.heightUntaggedOpt
    val networkHeightAtStart        = blockchainApi.getCurrentBlockchainHeight()
    val localOrNetworkHeightAtStart = localHeightAtStart.getOrElse(networkHeightAtStart)
    log.info(s"Current height: shared (local or network)=$localHeightAtStart, network=$networkHeightAtStart")

    val requestService = new DefaultRequestService(
      settings.rideRunner.requestsService,
      sharedBlockchain,
      allTags,
      new SynchronizedJobScheduler()(rideScheduler),
      rideScheduler
    )
    cs.cleanup(CustomShutdownPhase.BlockchainUpdatesStream) { requestService.close() }

    // Start from this height
    val lastSafeKnownHeight = Height(math.max(1, localOrNetworkHeightAtStart - 100 - 1)) // A rollback is not possible

    // When to run scripts
    val workingHeight = Height(math.max(localOrNetworkHeightAtStart, networkHeightAtStart))

    // When to stop the app
    val endHeight = Height(workingHeight + 1) // 101 // lastHeightAtStart

    log.info(s"Watching blockchain updates...")
    val blockchainUpdatesStream = blockchainApi.mkBlockchainUpdatesStream(blockchainEventsStreamScheduler)
    cs.cleanup(CustomShutdownPhase.BlockchainUpdatesStream) {
      blockchainUpdatesStream.close()
    }

    val processor = new BlockchainProcessor(sharedBlockchain, requestService)

    val scriptsTask = Task.parTraverseUnordered(scripts)(request => requestService.trackAndRun(request).map((request, _)))
    val events = blockchainUpdatesStream.downstream
      .doOnError(e =>
        Task {
          log.error("Error!", e)
        }
      )
      .takeWhile {
        case WrappedEvent.Next(_) => true
        case WrappedEvent.Closed =>
          log.info("Blockchain stream closed")
          false
        case WrappedEvent.Failed(error) =>
          log.error("Blockchain stream failed", error)
          false
      }
      .collect { case WrappedEvent.Next(event) => event }
      .scanEval(Task.now[BlockchainState](BlockchainState.Starting(lastSafeKnownHeight, workingHeight))) { case (origState, evt) =>
        val updatedState = BlockchainState(processor, origState, evt)
        (origState, updatedState) match {
          case (_: BlockchainState.Starting, _: BlockchainState.Working) => scriptsTask.runToFuture(rideScheduler)
          case _                                                         =>
        }
        Task.now(updatedState)
      }
      .lastL
      .runToFuture(blockchainEventsStreamScheduler)

    blockchainUpdatesStream.start(Height(lastSafeKnownHeight + 1), endHeight)

    Await.result(events, Duration.Inf)
    log.info("Running scripts...")

    val task = scriptsTask.runToFuture(rideScheduler)
    val resultsStr = Await
      .result(task, Duration.Inf)
      .map { case (request, result) => s"${request.detailedLogPrefix}: status=${result.lastStatus}, result=${result.lastResult}" }
      .mkString("\n")
    log.info(s"Results:\n$resultsStr")

    log.info("Done")
    cs.forceStop()
  }
}
