package com.wavesplatform.ride.runner.requests

import akka.http.scaladsl.model.StatusCodes
import com.github.benmanes.caffeine.cache.{Caffeine, Expiry, RemovalCause}
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.api.http.utils.UtilsEvaluator
import com.wavesplatform.api.http.utils.UtilsEvaluator.Evaluation
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.ride.runner.blockchain.ProxyBlockchain
import com.wavesplatform.ride.runner.environments.{DefaultDAppEnvironmentTracker, TrackedDAppEnvironment}
import com.wavesplatform.ride.runner.stats.RideRunnerStats.*
import com.wavesplatform.ride.runner.stats.{KamonCaffeineStats, RideRunnerStats}
import com.wavesplatform.ride.runner.storage.{CacheKey, SharedBlockchainStorage}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import play.api.libs.json.JsObject

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.SetHasAsScala

trait RequestService extends AutoCloseable {
  def start(): Unit
  def scheduleAffected(affected: Set[RideScriptRunRequest]): Unit
  def trackAndRun(request: RideScriptRunRequest): Task[RideScriptRunResult]
}

class DefaultRequestService(
    settings: DefaultRequestService.Settings,
    sharedBlockchain: SharedBlockchainStorage[RideScriptRunRequest],
    requestScheduler: JobScheduler[RideScriptRunRequest],
    runScriptScheduler: Scheduler
) extends RequestService
    with ScorexLogging {
  private val isWorking           = new AtomicBoolean(true)
  @volatile private var queueTask = CancelableFuture.unit

  private val ignoredRequests = ConcurrentHashMap.newKeySet[RideScriptRunRequest]()

  private def ignore(request: RideScriptRunRequest): Unit       = ignoredRequests.add(request)
  private def isIgnored(request: RideScriptRunRequest): Boolean = ignoredRequests.contains(request)
  private def removeFromIgnored(request: RideScriptRunRequest): Boolean = {
    val r = isIgnored(request)
    ignoredRequests.remove(request)
    r
  }

  private val requestsExpiry = new Expiry[RideScriptRunRequest, RideScriptRunResult] {
    private val duration = settings.cacheTtl.toNanos

    override def expireAfterRead(key: RideScriptRunRequest, value: RideScriptRunResult, currentTime: Long, currentDuration: Long): Long =
      duration
    override def expireAfterCreate(key: RideScriptRunRequest, value: RideScriptRunResult, currentTime: Long): Long =
      duration

    // Not modify the current expiration time
    override def expireAfterUpdate(key: RideScriptRunRequest, value: RideScriptRunResult, currentTime: Long, currentDuration: Long): Long =
      currentDuration
  }

  private val requests = Caffeine
    .newBuilder()
    .maximumSize(800) // TODO #96 Weighed cache
    .expireAfter(requestsExpiry)
    .evictionListener { (key: RideScriptRunRequest, _: RideScriptRunResult, _: RemovalCause) => ignore(key) }
    .recordStats(() => new KamonCaffeineStats("Requests"))
    .build[RideScriptRunRequest, RideScriptRunResult]()

  private val currJobs = new ConcurrentHashMap[RideScriptRunRequest, RequestJob]()

  override def start(): Unit = if (isWorking.get()) {
    log.info("Starting queue...")
    queueTask = requestScheduler.jobs
      .takeWhile(_ => isWorking.get())
      .filterNot(isIgnored)
      .mapParallelUnordered(settings.parallelization) { request =>
        log.debug(s"Processing ${request.shortLogPrefix}")
        if (createJob(request).inProgress) Task.unit
        else
          Option(currJobs.computeIfPresent(request, (_, x) => x.proceed())) match {
            case Some(updated) =>
              if (updated.isAvailable) {
                // We don't need to cache an empty value, so we use getOrElse here
                val origEnv = Option(requests.policy().getIfPresentQuietly(request)).getOrElse(RideScriptRunResult(request))
                runOne(origEnv).tapEval { r =>
                  Task {
                    requests.put(r.request, r)
                    updated.result.trySuccess(r)
                    Option(currJobs.remove(request)).foreach(_.timer.stop())
                  }
                }
              } else {
                log.info(s"$request is already running")
                requestScheduler.add(request) // A job is being processed now, but it can use stale data
                Task.unit
              }

            case _ => Task.unit
          }
      }
      .onErrorHandleWith { ex =>
        if (isWorking.get()) {
          log.error(s"Error observing item", ex)
          Observable.raiseError(ex)
        } else Observable.unit
      }
      .lastL
      .as(())
      .runToFuture(runScriptScheduler)
  }

  override def close(): Unit = if (isWorking.compareAndSet(true, false)) {
    queueTask.cancel()
    requestScheduler.close()
  }

  override def scheduleAffected(affected: Set[RideScriptRunRequest]): Unit = {
    val toRun = if (ignoredRequests.size() >= settings.ignoredCleanupThreshold) {
      sharedBlockchain.removeTags(ignoredRequests.asScala)
      ignoredRequests.clear()
      affected
    } else affected.filterNot(isIgnored)

    RideRunnerStats.requestServiceIgnoredNumber.update(ignoredRequests.size())
    RideRunnerStats.rideRequestActiveAffectedNumberByTypes.update(toRun.size.toDouble)

    log.info(f"Affected: total=${affected.size}, to run=${toRun.size} (${toRun.size * 100.0f / affected.size}%.1f%%)")
    requestScheduler.addMultiple(toRun)
    RideRunnerStats.rideRequestActiveNumber.update((requests.estimatedSize() - ignoredRequests.size()).toDouble)
  }

  override def trackAndRun(request: RideScriptRunRequest): Task[RideScriptRunResult] = Option(requests.getIfPresent(request)) match {
    case Some(cache) =>
      RideRunnerStats.rideRequestCacheHits.increment()
      Task.now(cache)

    case _ =>
      RideRunnerStats.rideRequestCacheMisses.increment()
      Option(currJobs.get(request)) match {
        case Some(job) => Task.fromCancelablePromise(job.result)
        case None =>
          Task {
            sharedBlockchain.getOrFetch(CacheKey.AccountScript(request.address))
          }.flatMap {
            case None => Task.now(fail(request, CustomValidationError(s"Address ${request.address} is not dApp")))
            case _ =>
              if (removeFromIgnored(request)) {
                RideRunnerStats.rideRequestTrackReAdded.increment()
                log.info(s"Re-added ${request.shortLogPrefix}")
              } else {
                RideRunnerStats.rideRequestTrackNew.increment()
                log.info(s"Added ${request.detailedLogPrefix}")
              }

              val job = createJob(request)
              requestScheduler.add(request, prioritized = true)
              Task.fromCancelablePromise(job.result)
          }.onErrorHandle { e =>
            log.warn("Can't run, got an error", e)
            fail(request, ApiError.Unknown)
          }
      }
  }

  private def createJob(request: RideScriptRunRequest): RequestJob = currJobs.computeIfAbsent(request, _ => RequestJob())

  private def runOne(prevResult: RideScriptRunResult): Task[RideScriptRunResult] =
    Task
      .evalAsync {
        try {
          val (evaluation, updatedResult) = RideRunnerStats.rideRequestRunTime.measure(evaluate(prevResult))

          val origResult      = prevResult.lastResult
          lazy val complexity = (updatedResult \ "complexity").as[Int]
          if ((updatedResult \ "error").isDefined) {
            log.trace(s"result=failed; ${prevResult.request.shortLogPrefix} errorCode=${(updatedResult \ "error").as[Int]}")
            RideRunnerStats.rideScriptFailedCalls.increment()
          } else if (
            origResult == JsObject.empty ||
            updatedResult \ "result" \ "value" != origResult \ "result" \ "value"
          ) {
            RideRunnerStats.rideScriptOkCalls.increment()
            log.trace(s"result=ok; ${prevResult.request.shortLogPrefix} complexity=$complexity")
          } else {
            RideRunnerStats.rideScriptUnnecessaryCalls.increment()
            log.trace(s"result=unnecessary; ${prevResult.request.shortLogPrefix} complexity=$complexity")
          }

          prevResult.copy(
            evaluation = evaluation,
            lastResult = updatedResult,
            lastStatus = StatusCodes.OK
          )
        } catch {
          case e: Throwable =>
            log.error(s"An error during running ${prevResult.request}: ${e.getMessage}")
            rideScriptFailedCalls.increment()
            fail(prevResult.request, CustomValidationError(e.getMessage))
        }
      }
      .executeOn(runScriptScheduler)

  private def evaluate(prevResult: RideScriptRunResult): (Option[Evaluation], JsObject) = {
    val (evaluation, failJson) =
      if (prevResult.evaluation.isDefined) (prevResult.evaluation, JsObject.empty)
      else // Note: the latest version here for simplicity
        RequestParser.parse(StdLibVersion.VersionDic.latest, prevResult.request.address, prevResult.request.requestBody) match {
          case Right(x) => (Some(x), JsObject.empty)
          case Left(e)  => (None, UtilsEvaluator.validationErrorToJson(e, settings.maxTxErrorLogSize))
        }

    evaluation match {
      case None => (evaluation, failJson)
      case Some(ev) =>
        val blockchain = new ProxyBlockchain(sharedBlockchain)
        val address    = prevResult.request.address
        val scriptInfo = blockchain.accountScript(address).getOrElse(throw new RuntimeException(s"There is no script on '$address'"))
        val jsResult = UtilsEvaluator.evaluate(
          settings.evaluateScriptComplexityLimit,
          blockchain,
          scriptInfo,
          ev,
          address,
          settings.enableTraces,
          settings.maxTxErrorLogSize,
          intAsString = true, // TODO #110 Int as string in evaluate
          wrapDAppEnv = underlying =>
            new TrackedDAppEnvironment(
              underlying,
              new DefaultDAppEnvironmentTracker(sharedBlockchain, prevResult.request)
            )
        )
        (evaluation, if (settings.enableStateChanges) jsResult else jsResult - "stateChanges")
    }
  }

  // TODO #19 Change/move an error to an appropriate layer
  private def fail(request: RideScriptRunRequest, reason: ApiError): RideScriptRunResult = RideScriptRunResult(
    request = request,
    evaluation = None,
    lastResult = reason.json,
    lastStatus = reason.code
  )
}

object DefaultRequestService {
  case class Settings(
      enableTraces: Boolean,
      enableStateChanges: Boolean,
      evaluateScriptComplexityLimit: Int,
      maxTxErrorLogSize: Int,
      parallelization: Int,
      cacheTtl: FiniteDuration,
      ignoredCleanupThreshold: Int
  )
}
