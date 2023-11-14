package com.wavesplatform.ride.runner.entrypoints

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import com.wavesplatform.account.Address
import com.wavesplatform.api.RideMulticastHttpApi
import com.wavesplatform.api.http.CompositeHttpService
import com.wavesplatform.ride.runner.http.{HttpServiceStatus, ServiceApiRoute}
import com.wavesplatform.ride.runner.stats.RideRunnerStats
import com.wavesplatform.utils.ScorexLogging
import io.netty.util.concurrent.DefaultThreadFactory
import kamon.Kamon
import kamon.instrumentation.executor.ExecutorInstrumentation
import monix.eval.Task
import monix.execution.{ExecutionModel, Scheduler}
import monix.reactive.Observable
import play.api.libs.json.{JsObject, Json}
import sttp.client3.HttpURLConnectionBackend

import java.io.File
import java.util.concurrent.{LinkedBlockingQueue, RejectedExecutionException, ThreadPoolExecutor, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}

object WavesRideRunnerCompareService extends ScorexLogging {
  case class Settings(
      requestsDelay: FiniteDuration,
      failedChecksToleranceTimer: FiniteDuration,
      maxChecks: Option[Long],
      rideApi: RideMulticastHttpApi.Settings,
      testRequests: List[(Address, JsObject)]
  )

  def main(args: Array[String]): Unit = {
    val (globalConfig, settings) = AppInitializer.init(checkDb = false, externalConfig = args.headOption.map(new File(_).getAbsoluteFile))
    if (settings.rideCompareService.testRequests.isEmpty) throw new IllegalArgumentException("Specify waves.compare.test-requests in config")

    log.info("Starting...")
    val metrics = new RideRunnerStats(globalConfig)

    implicit val actorSystem = ActorSystem("ride-runner", globalConfig)
    val cs                   = new Cleanup(actorSystem)
    cs.cleanup(CustomShutdownPhase.Metrics) { metrics.close() }

    val httpBackend = HttpURLConnectionBackend()
    cs.cleanupTask(CustomShutdownPhase.ApiClient, "httpBackend") { httpBackend.close() }

    val rideApi = new RideMulticastHttpApi(settings.rideCompareService.rideApi, httpBackend)

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

    val scheduler = mkScheduler("probes", 3)
    log.info(s"Found ${settings.rideCompareService.testRequests.size} scripts")
    val task: Task[Boolean] = Task
      .traverse(settings.rideCompareService.testRequests) { case (address, request) =>
        rideApi
          .ask(address, request)
          .map { x =>
            val id = s"[$address, $request]"
            log.info(s"Running $id")
            val r = x.rideRunner.response == x.node.response
            if (r) log.info(s"Same: node in ${x.node.time}, runner in ${x.rideRunner.time}, diff=${x.timeDiff}")
            else {
              val node = x.node.response.map { x =>
                (x \ "result" \ "value" \ "_2" \ "value").getOrElse(x)
              }
              val rideRunner = x.rideRunner.response.map { x =>
                (x \ "result" \ "value" \ "_2" \ "value").getOrElse(x)
              }
              log.warn(s"Different $id (time diff=${x.timeDiff}):\nnode in ${x.node.time}: $node\nride in ${x.rideRunner.time}: $rideRunner")
            }
            r
          }
      }
      .map(_.forall(_ == true))
      .onErrorRecoverWith { e =>
        Task {
          log.error("Error", e)
          false
        }
      }

    def now: Long                   = scheduler.clockMonotonic(TimeUnit.MILLISECONDS)
    @volatile var lastServiceStatus = ServiceStatus()
    val loop = {
      val s = Observable
        .intervalWithFixedDelay(settings.rideCompareService.requestsDelay)
        .mapEval(_ => task)
        .scan(now) { case (lastSuccessTs, lastCheckSuccess) =>
          val n = now
          lastServiceStatus = lastServiceStatus.copy(lastProcessedTimeMs = n, rideRunnerHealthy = true)
          if (lastCheckSuccess) {
            lastServiceStatus = lastServiceStatus.copy(lastSuccessTimeMs = n)
            n
          } else {
            if (n - lastSuccessTs > settings.rideCompareService.failedChecksToleranceTimer.toMillis) {
              lastServiceStatus = lastServiceStatus.copy(rideRunnerHealthy = false)
              log.error(s"Failed since $lastSuccessTs")
            }
            lastSuccessTs
          }
        }

      settings.rideCompareService.maxChecks
        .fold(s)(s.take)
        .lastL
        .runToFuture(scheduler)
    }
    cs.cleanup(CustomShutdownPhase.BlockchainUpdatesStream)(loop.cancel())

    log.info(s"Initializing REST API on ${settings.restApi.bindAddress}:${settings.restApi.port}...")
    val apiRoutes = Seq(
      ServiceApiRoute(
        { () =>
          val nowMs      = scheduler.clockMonotonic(TimeUnit.MILLISECONDS)
          val idleTimeMs = nowMs - lastServiceStatus.lastProcessedTimeMs
          HttpServiceStatus(
            healthy = true, // Because we don't want to affect this container
            debug = Json.obj(
              "rideRunnerHealthy" -> lastServiceStatus.rideRunnerHealthy,
              "nowTime"           -> nowMs,
              "lastProcessedTime" -> lastServiceStatus.lastProcessedTimeMs,
              "lastSuccessTime"   -> lastServiceStatus.lastProcessedTimeMs,
              "idleTime"          -> idleTimeMs
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
    log.info(Await.result(loop, Duration.Inf).toString)

    log.info("Done")
    cs.forceStop()
  }

  private case class ServiceStatus(
      rideRunnerHealthy: Boolean = false,
      nowTimeMs: Long = 0,
      lastProcessedTimeMs: Long = 0,
      lastSuccessTimeMs: Long = 0,
      idleTimeMs: Long = 0
  )
}
