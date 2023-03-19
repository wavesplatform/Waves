package com.wavesplatform.ride.runner.app

import akka.actor.ActorSystem
import com.wavesplatform.api.{DefaultBlockchainApi, GrpcChannelSettings, GrpcConnector}
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.ride.runner.db.RideDb
import com.wavesplatform.ride.runner.storage.persistent.{DefaultPersistentCaches, PersistentStorage}
import com.wavesplatform.ride.runner.storage.{RequestsStorage, ScriptRequest, SharedBlockchainStorage}
import com.wavesplatform.ride.runner.{BlockchainProcessor, BlockchainState, DefaultRequestService}
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
    val blockchainStorage = storage.readWrite { implicit rw =>
      val dbCaches = DefaultPersistentCaches(storage)
      SharedBlockchainStorage[ScriptRequest](settings.rideRunner.sharedBlockchain, storage, dbCaches, blockchainApi)
    }

    val lastHeightAtStart = Height(blockchainApi.getCurrentBlockchainHeight())
    log.info(s"Current height: known=${blockchainStorage.height}, blockchain=$lastHeightAtStart")

    val requestsService = new DefaultRequestService(
      settings.rideRunner.requestsService,
      storage,
      blockchainStorage,
      new RequestsStorage {
        override def size: Int                      = scripts.size
        override def all(): List[ScriptRequest]     = scripts
        override def append(x: ScriptRequest): Unit = {} // Ignore, because no way to evaluate a new expr
      },
      rideScheduler
    )

    // TODO #100 Fix App with BlockchainUpdates
    // log.info("Warming up caches...") // Helps to figure out, which data is used by a script
    // Await.result(requestsService.runAll().runToFuture(rideScheduler), Duration.Inf)

    // TODO #100 Settings?
    // mainnet
    val lastSafeKnownHeight = Height(3393500)                 // math.max(0, blockchainStorage.height - 100 - 1)) // A rollback is not possible
    val workingHeight       = Height(lastSafeKnownHeight + 3) // Height(math.max(blockchainStorage.height, lastHeightAtStart))
    val endHeight           = Height(workingHeight + 1)       // 101 // lastHeightAtStart

    // TODO #100 Settings?
    // testnet
    //      val lastKnownHeight = Height(2327973)
    //      val endHeight   = Height(lastKnownHeight + 1)

    log.info(s"Watching blockchain updates...")
    val blockchainUpdates = blockchainApi.mkBlockchainUpdatesStream(blockchainEventsStreamScheduler)
    cs.cleanup(CustomShutdownPhase.BlockchainUpdatesStream) {
      blockchainUpdates.close()
    }

    val processor = new BlockchainProcessor(blockchainStorage, requestsService)
    val events = blockchainUpdates.downstream
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
      .scanEval(Task.now[BlockchainState](BlockchainState.Starting(lastSafeKnownHeight, workingHeight)))(BlockchainState(processor, _, _))
      .lastL
      .runToFuture(blockchainEventsStreamScheduler)

    blockchainUpdates.start(lastSafeKnownHeight + 1, endHeight)

    log.info("Initialization completed")
    Await.result(events, Duration.Inf)

    log.info("Done")
    cs.forceStop()
  }
}
