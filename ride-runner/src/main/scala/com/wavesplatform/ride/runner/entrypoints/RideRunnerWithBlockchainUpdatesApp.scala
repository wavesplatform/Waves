package com.wavesplatform.ride.runner.entrypoints

import akka.actor.ActorSystem
import com.wavesplatform.api.{DefaultBlockchainApi, GrpcChannelSettings, GrpcConnector}
import com.wavesplatform.ride.runner.DefaultRequestService
import com.wavesplatform.ride.runner.db.RideDb
import com.wavesplatform.ride.runner.stats.RideRunnerStats
import com.wavesplatform.ride.runner.storage.persistent.{DefaultPersistentCaches, PersistentStorage}
import com.wavesplatform.ride.runner.storage.{RequestsStorage, ScriptRequest, SharedBlockchainStorage}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.ManagedChannel
import io.netty.util.concurrent.DefaultThreadFactory
import kamon.instrumentation.executor.ExecutorInstrumentation
import monix.execution.{ExecutionModel, Scheduler}
import play.api.libs.json.Json
import sttp.client3.HttpURLConnectionBackend

import java.io.File
import java.util.concurrent.{LinkedBlockingQueue, RejectedExecutionException, ThreadPoolExecutor, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}
import scala.io.Source
import scala.util.Using

object RideRunnerWithBlockchainUpdatesApp extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val (globalConfig, settings) = AppInitializer.init(args.headOption.map(new File(_)))

    log.info("Loading args...")
    val inputFile =
      if (args.length < 2) throw new IllegalArgumentException("Please specify an input.json file")
      else new File(args(1))

    val scripts = Json
      .parse(Using(Source.fromFile(inputFile))(_.getLines().mkString("\n")).get)
      .as[List[ScriptRequest]]

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
    val httpBackend = HttpURLConnectionBackend()
    cs.cleanupTask(CustomShutdownPhase.ApiClient, "httpBackend") {
      httpBackend.close()
    }

    val blockchainApi = new DefaultBlockchainApi(
      settings = settings.rideRunner.blockchainApi,
      grpcApiChannel = grpcApiChannel,
      blockchainUpdatesApiChannel = blockchainUpdatesApiChannel,
      httpBackend = httpBackend
    )

    log.info("Opening a caches DB...")
    val db = RideDb.open(settings.rideRunner.db).db
    cs.cleanup(CustomShutdownPhase.Db) { db.close() }

    val storage = PersistentStorage.rocksDb(db)
    val sharedBlockchain = storage.readWrite { implicit rw =>
      val dbCaches = DefaultPersistentCaches(storage)
      SharedBlockchainStorage[ScriptRequest](settings.rideRunner.sharedBlockchain, storage, dbCaches, blockchainApi)
    }

    val requestsService = new DefaultRequestService(
      settings.rideRunner.requestsService,
      storage,
      sharedBlockchain,
      new RequestsStorage {
        override def size: Int                      = scripts.size
        override def all(): List[ScriptRequest]     = scripts
        override def append(x: ScriptRequest): Unit = {} // Ignore, because no way to evaluate a new expr
      },
      rideScheduler
    )

    // TODO #100 Fix App with BlockchainUpdates
    // log.info("Warming up caches...") // Helps to figure out, which data is used by a script
    Await.result(requestsService.trackAndRun(scripts.head).runToFuture(rideScheduler), Duration.Inf)

    log.info("Done")
    cs.forceStop()
  }
}
