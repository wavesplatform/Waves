package com.wavesplatform.ride.runner.requests

import com.github.benmanes.caffeine.cache.Caffeine
import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.api.http.ApiException
import com.wavesplatform.api.http.utils.UtilsApiRoute
import com.wavesplatform.ride.runner.blockchain.ProxyBlockchain
import com.wavesplatform.ride.runner.environments.{DefaultDAppEnvironmentTracker, TrackedDAppEnvironment}
import com.wavesplatform.ride.runner.requests.DefaultRequestService.RideScriptRunEnvironment
import com.wavesplatform.ride.runner.stats.RideRunnerStats.*
import com.wavesplatform.ride.runner.stats.{KamonCaffeineStats, RideRunnerStats}
import com.wavesplatform.ride.runner.storage.{CacheKey, ScriptRequest, SharedBlockchainStorage}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.json.JsObject

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.FiniteDuration

trait RequestService extends AutoCloseable {
  def start(): Unit
  def runAffected(affected: Set[ScriptRequest]): Task[Unit]
  def trackAndRun(request: ScriptRequest): Task[JsObject]
}

class DefaultRequestService(
    settings: DefaultRequestService.Settings,
    sharedBlockchain: SharedBlockchainStorage[ScriptRequest],
    requestScheduler: JobScheduler[ScriptRequest],
    runScriptScheduler: Scheduler
) extends RequestService
    with ScorexLogging {
  private val isWorking = new AtomicBoolean(true)

  private val requests = Caffeine
    .newBuilder()
    .maximumSize(10000) // TODO #96 Weighed cache
    .expireAfterWrite(settings.cacheTtl.length, settings.cacheTtl.unit)
    .recordStats(() => new KamonCaffeineStats("Requests"))
    .build[ScriptRequest, RideScriptRunEnvironment]()

  private val currJobs = new ConcurrentHashMap[ScriptRequest, RequestJob]()

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
                    val origEnv = Option(requests.getIfPresent(request)).getOrElse(RideScriptRunEnvironment(request))
                    runOne(sharedBlockchain.heightUntagged, origEnv).tapEval { r =>
                      Task {
                        requests.put(r.key, r)
                        updated.result.trySuccess(r.lastResult)
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

  override def runAffected(affected: Set[ScriptRequest]): Task[Unit] = Task {
    requestScheduler.addMultiple(affected)
    RideRunnerStats.rideRequestTotalNumber.update(requests.estimatedSize().toDouble)
  }

  override def trackAndRun(request: ScriptRequest): Task[JsObject] = Option(requests.getIfPresent(request)) match {
    case Some(cache) =>
      rideRequestCacheHits.increment()
      Task.now(cache.lastResult)

    case _ =>
      rideRequestCacheMisses.increment()
      Option(currJobs.get(request)) match {
        case Some(job) => Task.fromCancelablePromise(job.result)
        case None =>
          Task {
            sharedBlockchain.getOrFetch(CacheKey.AccountScript(request.address))
          }.flatMap {
            // TODO #19 Change/move an error to an appropriate layer
            case None => Task.raiseError(ApiException(CustomValidationError(s"Address ${request.address} is not dApp")))
            case _ =>
              log.info(s"Added ${request.detailedLogPrefix}")
              val job = createJob(request)
              requestScheduler.add(request, prioritized = true)
              Task.fromCancelablePromise(job.result)
          }
      }
  }

  private def createJob(request: ScriptRequest): RequestJob = currJobs.computeIfAbsent(request, _ => RequestJob.mk())

  private def runOne(height: Int, scriptEnv: RideScriptRunEnvironment): Task[RideScriptRunEnvironment] =
    Task
      .evalAsync {
        val updatedResult = rideRequestRunTime.measure {
          evaluate(scriptEnv) - "stateChanges" // Not required for now
        }

        val origResult      = scriptEnv.lastResult
        lazy val complexity = (updatedResult \ "complexity").as[Int]
        if ((updatedResult \ "error").isDefined) {
          log.trace(s"result=failed; ${scriptEnv.key.shortLogPrefix} errorCode=${(updatedResult \ "error").as[Int]}")
          rideScriptFailedCalls.increment()
        } else if (
          origResult == JsObject.empty ||
          updatedResult \ "result" \ "value" != origResult \ "result" \ "value"
        ) {
          rideScriptOkCalls.increment()
          log.trace(s"result=ok; ${scriptEnv.key.shortLogPrefix} complexity=$complexity")
        } else {
          rideScriptUnnecessaryCalls.increment()
          log.trace(s"result=unnecessary; ${scriptEnv.key.shortLogPrefix} complexity=$complexity")
        }

        scriptEnv.copy(lastResult = updatedResult, updateHeight = height)
      }
      .tapError { e => Task(log.error(s"An error during running ${scriptEnv.key}", e)) }
      .executeOn(runScriptScheduler)

  private def evaluate(script: RideScriptRunEnvironment): JsObject = UtilsApiRoute.evaluate(
    settings.evaluateScriptComplexityLimit,
    new ProxyBlockchain(sharedBlockchain),
    script.key.address,
    script.key.requestBody,
    settings.enableTraces,
    settings.maxTxErrorLogSize,
    intAsString = true, // TODO #110 Int as string in evaluate
    wrapDAppEnv = underlying =>
      new TrackedDAppEnvironment(
        underlying,
        new DefaultDAppEnvironmentTracker(sharedBlockchain, script.key)
      )
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

  private final case class RideScriptRunEnvironment(key: ScriptRequest, lastResult: JsObject, updateHeight: Int)

  private object RideScriptRunEnvironment {
    def apply(key: ScriptRequest): RideScriptRunEnvironment = new RideScriptRunEnvironment(key, JsObject.empty, 0)
  }
}
