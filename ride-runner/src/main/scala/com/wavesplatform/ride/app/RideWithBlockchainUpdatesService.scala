package com.wavesplatform.ride.app

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import com.wavesplatform.api.http.{CompositeHttpService, RouteTimeout}
import com.wavesplatform.blockchain.{BlockchainProcessor, BlockchainState, SharedBlockchainData}
import com.wavesplatform.database.openDB
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.grpc.{DefaultBlockchainApi, GrpcConnector}
import com.wavesplatform.http.EvaluateApiRoute
import com.wavesplatform.resources.*
import com.wavesplatform.state.Height
import com.wavesplatform.storage.LevelDbRequestsStorage
import com.wavesplatform.storage.RequestsStorage.RequestKey
import com.wavesplatform.storage.persistent.LevelDbPersistentCaches
import com.wavesplatform.utils.ScorexLogging
import io.netty.util.concurrent.DefaultThreadFactory
import kamon.instrumentation.executor.ExecutorInstrumentation
import monix.eval.Task
import monix.execution.{ExecutionModel, Scheduler}
import sttp.client3.HttpURLConnectionBackend

import java.io.File
import java.util.concurrent.*
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}
import scala.util.Failure

object RideWithBlockchainUpdatesService extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val (globalConfig, settings) = AppInitializer.init(args.headOption.map(new File(_)))

    val r = Using.Manager { use =>
      use.acquire(new Metrics(globalConfig))

      log.info("Initializing thread pools...")

      def mkScheduler(name: String, threads: Int): Scheduler = {
        val executor = use(
          new ThreadPoolExecutor(
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
        )

        use.acquireWithShutdown(
          Scheduler(
            executor = if (globalConfig.getBoolean("kamon.enable")) ExecutorInstrumentation.instrument(executor, name) else executor,
            executionModel = ExecutionModel.AlwaysAsyncExecution
          )
        ) { resource =>
          resource.shutdown()
          resource.awaitTermination(5.seconds)
        }
      }

      val blockchainEventsStreamScheduler = mkScheduler("blockchain-events", 2)
      val rideRequestsScheduler = mkScheduler(
        name = "ride",
        threads = settings.restApi.heavyRequestProcessorPoolThreads.getOrElse((Runtime.getRuntime.availableProcessors() * 2).min(4))
      )

      val connector = use(new GrpcConnector(settings.rideRunner.grpcConnector))

      log.info("Making gRPC channel to gRPC API...")
      val grpcApiChannel = use(connector.mkChannel(settings.rideRunner.grpcApiChannel))

      log.info("Making gRPC channel to Blockchain Updates API...")
      val blockchainUpdatesApiChannel = use(connector.mkChannel(settings.rideRunner.blockchainUpdatesApiChannel))

      val httpBackend = use.acquireWithShutdown(HttpURLConnectionBackend())(_.close())

      val blockchainApi = new DefaultBlockchainApi(
        settings = settings.rideRunner.blockchainApi,
        grpcApiChannel = grpcApiChannel,
        blockchainUpdatesApiChannel = blockchainUpdatesApiChannel,
        httpBackend = httpBackend
      )

      val db                = use(openDB(settings.rideRunner.db.directory))
      val dbCaches          = new LevelDbPersistentCaches(db)
      val blockchainStorage = new SharedBlockchainData[RequestKey](settings.blockchain, dbCaches, blockchainApi)

      val lastHeightAtStart = Height(blockchainApi.getCurrentBlockchainHeight())
      log.info(s"Current height: $lastHeightAtStart")

      val requestsStorage = new LevelDbRequestsStorage(db)
      val processor = new BlockchainProcessor(
        settings.rideRunner.processor,
        blockchainStorage,
        requestsStorage,
        blockchainEventsStreamScheduler
      )

      log.info("Warm up caches...") // Also helps to figure out, which data is used by a script
      processor.runScripts(forceAll = true)

      val lastKnownHeight = Height(math.max(0, blockchainStorage.height - 100 - 1))
      val workingHeight   = Height(blockchainStorage.height)

      val blockchainUpdates = use(blockchainApi.mkBlockchainUpdatesStream(blockchainEventsStreamScheduler))
      // TODO #33 Move wrapped events from here: processing of Closed and Failed should be moved to blockchainUpdates.stream
      val events = blockchainUpdates.stream
        .doOnError(e => Task { log.error("Error!", e) })
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
        // TODO #38 use scanLeft*
        .foldLeftL(BlockchainState.Starting(workingHeight): BlockchainState)(BlockchainState(processor, _, _))
        .runToFuture(blockchainEventsStreamScheduler)

      log.info(s"Watching blockchain updates...")
      blockchainUpdates.start(lastKnownHeight + 1)

      val routeTimeout = new RouteTimeout(
        FiniteDuration(globalConfig.getDuration("akka.http.server.request-timeout").getSeconds, TimeUnit.SECONDS)
      )(rideRequestsScheduler)

      val apiRoutes = Seq(
        EvaluateApiRoute(
          routeTimeout,
          Function.tupled(processor.getCachedResultOrRun)
        )
      )

      implicit val actorSystem = use.acquireWithShutdown(ActorSystem("ride-runner", globalConfig)) { x =>
        Await.ready(x.terminate(), 20.seconds)
      }
      val httpService = CompositeHttpService(apiRoutes, settings.restApi)
      val httpFuture = Http()
        .newServerAt(settings.restApi.bindAddress, settings.restApi.port)
        .bindFlow(httpService.loggingCompositeRoute)

      log.info(s"REST API binging on ${settings.restApi.bindAddress}:${settings.restApi.port}")
      Await.result(httpFuture, 20.seconds)
      log.info("REST API was bound")

      Await.result(events, Duration.Inf)
    }

    r match {
      case Failure(e) => log.error("Got an error", e)
      case _          => log.info("Done")
    }
  }
}
