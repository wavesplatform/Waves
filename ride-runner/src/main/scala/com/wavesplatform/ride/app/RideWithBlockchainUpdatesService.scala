package com.wavesplatform.ride.app

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import cats.syntax.either.*
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.CompositeHttpService
import com.wavesplatform.blockchain.BlockchainProcessor.RequestKey
import com.wavesplatform.blockchain.{BlockchainProcessor, BlockchainState, SharedBlockchainData}
import com.wavesplatform.database.openDB
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.grpc.{DefaultBlockchainApi, GrpcConnector}
import com.wavesplatform.http.EvaluateApiRoute
import com.wavesplatform.resources.*
import com.wavesplatform.state.Height
import com.wavesplatform.storage.persistent.LevelDbPersistentCaches
import com.wavesplatform.utils.ScorexLogging
import io.netty.util.concurrent.DefaultThreadFactory
import monix.eval.Task
import monix.execution.{ExecutionModel, Scheduler}
import monix.reactive.subjects.ConcurrentSubject
import monix.reactive.{MulticastStrategy, Observable, OverflowStrategy}
import play.api.libs.json.JsObject
import sttp.client3.HttpURLConnectionBackend

import java.io.File
import java.util.concurrent.*
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, Promise}
import scala.util.Failure
import scala.util.control.NoStackTrace

object RideWithBlockchainUpdatesService extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val basePath                 = args(0)
    val (globalConfig, settings) = AppInitializer.init(Some(new File(s"$basePath/node/waves.conf")))

    val r = Using.Manager { use =>
      val connector = use(new GrpcConnector(settings.rideRunner.grpcConnector))

      log.info("Making gRPC channel to gRPC API...")
      val grpcApiChannel = use(connector.mkChannel(settings.rideRunner.grpcApi))

      log.info("Making gRPC channel to Blockchain Updates API...")
      val blockchainUpdatesApiChannel = use(connector.mkChannel(settings.rideRunner.blockchainUpdatesApi))

      val commonScheduler = use(
        Executors.newScheduledThreadPool(
          2,
          new ThreadFactoryBuilder().setNameFormat("common-scheduler-%d").setDaemon(false).build()
        )
      )

      val monixScheduler = use.acquireWithShutdown(Scheduler(commonScheduler).withExecutionModel(ExecutionModel.AlwaysAsyncExecution)) { x =>
        x.shutdown()
        x.awaitTermination(5.seconds)
      }

      val httpBackend = use.acquireWithShutdown(HttpURLConnectionBackend())(_.close())

      val blockchainApi = new DefaultBlockchainApi(
        settings = settings.rideRunner.blockchainApi,
        grpcApiChannel = grpcApiChannel,
        blockchainUpdatesApiChannel = blockchainUpdatesApiChannel,
        httpBackend = httpBackend,
        scheduler = monixScheduler
      )

      val db                = use(openDB(s"$basePath/db"))
      val dbCaches          = new LevelDbPersistentCaches(db)
      val blockchainStorage = new SharedBlockchainData[RequestKey](settings.blockchain, dbCaches, blockchainApi)

      val lastHeightAtStart = Height(blockchainApi.getCurrentBlockchainHeight())
      log.info(s"Current height: $lastHeightAtStart")

      val processor = BlockchainProcessor.mk(
        settings.rideRunner.processor,
        monixScheduler,
        blockchainStorage
      )

      log.info("Warm up caches...") // Also helps to figure out, which data is used by a script
      processor.runScripts(forceAll = true)

//      val lastKnownHeight             = Height(math.max(0, blockchainStorage.height - 100 - 1))
      val lastKnownHeight = Height(1)
      val workingHeight   = Height(lastKnownHeight + 3)
      val endHeight       = workingHeight + 1

      val blockchainUpdates = use(blockchainApi.mkBlockchainUpdatesStream())
      val toExecute         = ConcurrentSubject[FullRequest](MulticastStrategy.publish)(monixScheduler)

      val maxRequestBufferSize = 1000 // TODO #10 settings
      val xs: List[Observable[WrappedEvent[Either[Seq[FullRequest], SubscribeEvent]]]] = List(
        blockchainUpdates.stream.map(_.map(_.asRight[Seq[FullRequest]])),
        // TODO #10 add a buffer and cancel new requests due to overflow
        toExecute
          .flatMap { x =>
            processor.getCachedResultOrRun(x.address, x.request) match {
              case RunResult.Cached(r) =>
                x.result.success(r)
                Observable.empty

              case _ => Observable(x)
            }
          }
          // Like .whileBusyBuffer(OverflowStrategy.DropNew(maxRequestBufferSize)), but has an access to the event
          .whileBusyAggregateEvents(Vector(_)) {
            case (buffer, curr) =>
              if (buffer.size < maxRequestBufferSize) buffer.appended(curr)
              else {
                curr.result.failure(ServerUnderLoadException)
                buffer
              }
          }
          .flatMapIterable(identity)
          // Buffering to limit a parallel execution of new scripts, otherwise the service can stuck and don't process blockchain events.
          // Execution of scripts is blocking, because Ride must know a returned value of a blockchain function to run further.
          // Also we can't run scripts outside this stream, because imagine:
          // 1. We receive a request
          // 2.
          .bufferTimedAndCounted(50.millis, 5) // TODO #10 settings
          .map(x => WrappedEvent.Next(x.asLeft[SubscribeEvent]))
      )

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
        .scanEval0F(Task.now(BlockchainState.Starting(workingHeight): BlockchainState)) { case (r, curr) =>
          curr match {
            case Right(curr) => Task.now(BlockchainState(processor, r, curr))
            case Left(xs) =>
              Task
                .parSequenceUnordered {
                  xs.map { x =>
                    processor.getCachedResultOrRun(x.address, x.request) match {
                      case RunResult.Cached(r) => Task.now(x.result.success(r))
                      case RunResult.NotProcessed(task) =>
                        task
                          .foreachL(x.result.success)
                          .doOnFinish(r => Task.now(r.foreach(x.result.failure)))
                          .doOnCancel(Task(x.result.tryFailure(new CancellationException("Task was cancelled"))))
                    }
                  }
                }
                .map(_ => r)
          }
        }
        .lastL
        .runToFuture(monixScheduler)

      log.info(s"Watching blockchain updates...")
      blockchainUpdates.start(lastKnownHeight + 1, endHeight)

      // ====
      val heavyRequestProcessorPoolThreads =
        /*settings.restAPISettings.heavyRequestProcessorPoolThreads*/ None.getOrElse((Runtime.getRuntime.availableProcessors() * 2).min(4))
      val heavyRequestExecutor = use(
        new ThreadPoolExecutor(
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
      )

      val heavyRequestScheduler = use.acquireWithShutdown(
        Scheduler(
          /*if (settings.config.getBoolean("kamon.enable"))
            ExecutorInstrumentation.instrument(heavyRequestExecutor, "heavy-request-executor")
          else*/ heavyRequestExecutor,
          ExecutionModel.AlwaysAsyncExecution
        )
      ) { resource =>
        resource.shutdown()
        resource.awaitTermination(5.seconds)
      }

      val apiRoutes = Seq(
        EvaluateApiRoute(
          heavyRequestScheduler,
          { (address, request) =>
            val result = Promise[JsObject]()
            toExecute.onNext(FullRequest(address, request, result))
            result.future
          }
        )
      )

      implicit val actorSystem = use.acquireWithShutdown(ActorSystem("ride-runner", globalConfig)) { x =>
        Await.ready(x.terminate(), 20.seconds)
      }
      val httpService = CompositeHttpService(apiRoutes, settings.restApi)
      val httpFuture = Http()
        .newServerAt(settings.restApi.bindAddress, settings.restApi.port)
        .bindFlow(httpService.loggingCompositeRoute)

      val _ = Await.result(httpFuture, 20.seconds)
      log.info(s"REST API was bound on ${settings.restApi.bindAddress}:${settings.restApi.port}")
      Await.result(events, Duration.Inf)
    }

    r match {
      case Failure(e) => log.error("Got an error", e)
      case _          => log.info("Done")
    }
  }
}

case class FullRequest(address: Address, request: JsObject, result: Promise[JsObject])
object ServerUnderLoadException extends RuntimeException("The server under the load, try later or contact the administrator") with NoStackTrace
