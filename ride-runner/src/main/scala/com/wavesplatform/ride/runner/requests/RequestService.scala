package com.wavesplatform.ride.runner.requests

import akka.http.scaladsl.model.StatusCodes
import com.github.benmanes.caffeine.cache.Caffeine
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.api.http.utils.UtilsApiRoute
import com.wavesplatform.ride.runner.blockchain.ProxyBlockchain
import com.wavesplatform.ride.runner.environments.{DefaultDAppEnvironmentTracker, TrackedDAppEnvironment}
import com.wavesplatform.ride.runner.stats.RideRunnerStats.*
import com.wavesplatform.ride.runner.stats.{KamonCaffeineStats, RideRunnerStats}
import com.wavesplatform.ride.runner.storage.{CacheKey, RideScriptRunRequest, SharedBlockchainStorage}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.json.JsObject

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.FiniteDuration

trait RequestService extends AutoCloseable {
  def start(): Unit
  def runAffected(affected: Set[RideScriptRunRequest]): Task[Unit]
  def trackAndRun(request: RideScriptRunRequest): Task[RideScriptRunResult]
}

class DefaultRequestService(
    settings: DefaultRequestService.Settings,
    sharedBlockchain: SharedBlockchainStorage[RideScriptRunRequest],
    requestScheduler: JobScheduler[RideScriptRunRequest],
    runScriptScheduler: Scheduler
) extends RequestService
    with ScorexLogging {
  private val isWorking = new AtomicBoolean(true)

  private val requests = Caffeine
    .newBuilder()
    .maximumSize(10000) // TODO #96 Weighed cache
    .expireAfterWrite(settings.cacheTtl.length, settings.cacheTtl.unit)
    .recordStats(() => new KamonCaffeineStats("Requests"))
    .build[RideScriptRunRequest, RideScriptRunResult]()

  private val currJobs = new ConcurrentHashMap[RideScriptRunRequest, RequestJob]()

  override def start(): Unit = {
    val task =
      if (!isWorking.get()) Task.unit
      else {
        log.info("Starting...")
        requestScheduler.jobs
          .takeWhile(_ => isWorking.get())
          .mapParallelUnordered(settings.parallelization) { request =>
            if (createJob(request).inProgress) Task.unit
            else
              Option(currJobs.computeIfPresent(request, (_, x) => x.proceed())) match {
                case Some(updated) =>
                  if (updated.isAvailable) {
                    // We don't need to cache an empty value, so we use getOrElse here
                    val origEnv = Option(requests.getIfPresent(request)).getOrElse(RideScriptRunResult(request))
                    runOne(origEnv).tapEval { r =>
                      Task {
                        requests.put(r.request, r)
                        updated.result.trySuccess(r)
                        currJobs.remove(request)
                      }
                    }
                  } else {
                    log.info(s"$request is already running")
                    Task(requestScheduler.add(request)) // A job is being processed now, but it can use stale data
                  }

                case _ => Task.unit
              }
          }
          .logErr
          .lastL
          .as(())
      }

    // TODO cancel?
    task.runToFuture(runScriptScheduler)
  }

  override def close(): Unit = {
    isWorking.set(false)
    requestScheduler.close()
//    Await.result(currTask)
  }

  override def runAffected(affected: Set[RideScriptRunRequest]): Task[Unit] = Task {
    requestScheduler.addMultiple(affected)
    RideRunnerStats.rideRequestActiveNumber.update(requests.estimatedSize().toDouble)
  }

  override def trackAndRun(request: RideScriptRunRequest): Task[RideScriptRunResult] = Option(requests.getIfPresent(request)) match {
    case Some(cache) =>
      rideRequestCacheHits.increment()
      Task.now(cache)

    case _ =>
      rideRequestCacheMisses.increment()
      Option(currJobs.get(request)) match {
        case Some(job) => Task.fromCancelablePromise(job.result)
        case None =>
          Task {
            sharedBlockchain.getOrFetch(CacheKey.AccountScript(request.address))
          }.flatMap {
            case None => Task.now(fail(request, CustomValidationError(s"Address ${request.address} is not dApp")))
            case _ =>
              log.info(s"Added ${request.detailedLogPrefix}")
              val job = createJob(request)
              requestScheduler.add(request, prioritized = true)
              Task.fromCancelablePromise(job.result)
          }.onErrorHandle { e =>
            log.warn("Can't run, got an error", e)
            fail(request, ApiError.Unknown)
          }
      }
  }

  private def createJob(request: RideScriptRunRequest): RequestJob = currJobs.computeIfAbsent(request, _ => RequestJob.mk())

  private def runOne(scriptEnv: RideScriptRunResult): Task[RideScriptRunResult] =
    Task
      .evalAsync {
        try {
          val updatedResult = rideRequestRunTime.measure {
            evaluate(scriptEnv.request) - "stateChanges" // Not required for now
          }

          val origResult      = scriptEnv.lastResult
          lazy val complexity = (updatedResult \ "complexity").as[Int]
          if ((updatedResult \ "error").isDefined) {
            log.trace(s"result=failed; ${scriptEnv.request.shortLogPrefix} errorCode=${(updatedResult \ "error").as[Int]}")
            rideScriptFailedCalls.increment()
          } else if (
            origResult == JsObject.empty ||
            updatedResult \ "result" \ "value" != origResult \ "result" \ "value"
          ) {
            rideScriptOkCalls.increment()
            log.trace(s"result=ok; ${scriptEnv.request.shortLogPrefix} complexity=$complexity")
          } else {
            rideScriptUnnecessaryCalls.increment()
            log.trace(s"result=unnecessary; ${scriptEnv.request.shortLogPrefix} complexity=$complexity")
          }

          scriptEnv.copy(
            lastResult = updatedResult,
            lastStatus = StatusCodes.OK
          )
        } catch {
          case e: Throwable =>
            log.error(s"An error during running ${scriptEnv.request}: ${e.getMessage}")
            rideScriptFailedCalls.increment()
            fail(scriptEnv.request, CustomValidationError(e.getMessage))
        }
      }
      .executeOn(runScriptScheduler)

  private def evaluate(request: RideScriptRunRequest): JsObject = UtilsApiRoute.evaluate(
    settings.evaluateScriptComplexityLimit,
    new ProxyBlockchain(sharedBlockchain),
    request.address,
    request.requestBody,
    settings.enableTraces,
    settings.maxTxErrorLogSize,
    intAsString = true, // TODO #110 Int as string in evaluate
    wrapDAppEnv = underlying =>
      new TrackedDAppEnvironment(
        underlying,
        new DefaultDAppEnvironmentTracker(sharedBlockchain, request)
      )
  )

  // TODO #19 Change/move an error to an appropriate layer
  private def fail(request: RideScriptRunRequest, reason: ApiError): RideScriptRunResult = RideScriptRunResult(
    request = request,
    lastResult = reason.json,
    lastStatus = reason.code
  )
}

object DefaultRequestService {
  case class Settings(
      enableTraces: Boolean,
      evaluateScriptComplexityLimit: Int,
      maxTxErrorLogSize: Int,
      parallelization: Int,
      cacheTtl: FiniteDuration
  )
}
