package com.wavesplatform.ride.runner.requests

import akka.http.scaladsl.model.StatusCodes
import cats.syntax.either.*
import com.github.benmanes.caffeine.cache.{Caffeine, Expiry, RemovalCause, Scheduler as CaffeineScheduler}
import com.typesafe.config.ConfigMemorySize
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.api.http.ApiError.{CustomValidationError, Unknown}
import com.wavesplatform.api.http.utils.{Evaluation, UtilsEvaluator}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.ride.runner.caches.mem.MemCacheWeights
import com.wavesplatform.ride.runner.caches.{CacheKeyTags, LazyBlockchain}
import com.wavesplatform.ride.runner.environments.{DefaultDAppEnvironmentTracker, TrackedDAppEnvironment}
import com.wavesplatform.ride.runner.stats.RideRunnerStats.*
import com.wavesplatform.ride.runner.stats.{KamonCaffeineStats, RideRunnerStats}
import com.wavesplatform.state.AccountScriptInfo
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.ScorexLogging
import io.grpc.{Status, StatusException, StatusRuntimeException}
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import play.api.libs.json.Json

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.{SetHasAsJava, SetHasAsScala}

trait RequestService extends AutoCloseable {
  def start(): Unit
  def scheduleAffected(affected: Set[RideScriptRunRequest]): Unit
  def trackAndRun(request: RideScriptRunRequest): Task[RideScriptRunResult]
}

class DefaultRequestService(
    settings: DefaultRequestService.Settings,
    blockchain: LazyBlockchain[RideScriptRunRequest],
    allTags: CacheKeyTags[RideScriptRunRequest],
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

  private def clearIgnored(): Unit = if (ignoredRequests.size() >= settings.ignoredCleanupThreshold) {
    val toRemove = ignoredRequests.asScala.toSet // copying
    allTags.removeTags(toRemove)
    ignoredRequests.removeAll(toRemove.asJava)
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

  private val activeRequests = Caffeine
    .newBuilder()
    .maximumWeight(settings.cacheSize.toBytes)
    .weigher { (key: RideScriptRunRequest, value: RideScriptRunResult) =>
      MemCacheWeights.ofRideScriptRunRequest(key) + MemCacheWeights.ofRideScriptRunResult(value)
    }
    .expireAfter(requestsExpiry)
    .scheduler(CaffeineScheduler.systemScheduler()) // Because we rely on eviction
    .evictionListener { (key: RideScriptRunRequest, _: RideScriptRunResult, _: RemovalCause) => ignore(key) }
    .recordStats(() => new KamonCaffeineStats("Requests"))
    .build[RideScriptRunRequest, RideScriptRunResult]()

  private val currJobs                                     = new ConcurrentHashMap[RideScriptRunRequest, RequestJob]()
  private def stopJob(request: RideScriptRunRequest): Unit = Option(currJobs.remove(request)).foreach(_.timer.stop())

  private def isActive(request: RideScriptRunRequest): Boolean =
    currJobs.containsKey(request) || Option(activeRequests.policy().getIfPresentQuietly(request)).isDefined

  override def start(): Unit = if (isWorking.get()) {
    log.info("Starting queue...")
    queueTask = requestScheduler.jobs
      .takeWhile(_ => isWorking.get())
      .doOnNext(_ => Task(clearIgnored())) // This is the only place where we run scripts and thus add tags.
      .filter(isActive)
      .mapParallelUnordered(settings.parallelRideRunThreads) { request =>
        log.debug(s"Processing ${request.shortLogPrefix}")
        if (createJob(request).inProgress) Task.unit
        else
          Option(currJobs.computeIfPresent(request, (_, x) => x.proceed())) match {
            case Some(updated) =>
              if (updated.isAvailable) {
                // We don't need to cache an empty value, so we use getOrElse here
                val origEnv = Option(activeRequests.policy().getIfPresentQuietly(request)).getOrElse(RideScriptRunResult())
                runOne(request, origEnv).redeem(
                  e => {
                    updated.result.tryFailure(e)
                    stopJob(request)
                  },
                  x => {
                    activeRequests.put(request, x)
                    updated.result.trySuccess(x)
                    stopJob(request)
                  }
                )
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
    val toRun = affected.filter(isActive)

    RideRunnerStats.requestServiceIgnoredNumber.update(ignoredRequests.size())
    RideRunnerStats.rideRequestActiveAffectedNumberByTypes.update(toRun.size.toDouble)

    log.info(f"Affected: total=${affected.size}, to run=${toRun.size} (${toRun.size * 100.0f / affected.size}%.1f%%)")
    requestScheduler.addMultiple(toRun)
    RideRunnerStats.rideRequestActiveNumber.update(activeRequests.estimatedSize().toDouble)
  }

  override def trackAndRun(request: RideScriptRunRequest): Task[RideScriptRunResult] = Option(activeRequests.getIfPresent(request)) match {
    case Some(cache) =>
      RideRunnerStats.rideRequestCacheHits.increment()
      Task.now(cache)

    case _ =>
      RideRunnerStats.rideRequestCacheMisses.increment()
      Option(currJobs.get(request)) match {
        case Some(job) => Task.fromCancelablePromise(job.result)
        case None =>
          Task {
            blockchain.accountScript(request.address)
          }.flatMap {
            case None => Task.now(fail(CustomValidationError(s"Address ${request.address} is not dApp")))
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
            fail(ApiError.Unknown)
          }
      }
  }

  private def createJob(request: RideScriptRunRequest): RequestJob = currJobs.computeIfAbsent(request, _ => RequestJob())

  private def runOne(request: RideScriptRunRequest, prevResult: RideScriptRunResult): Task[RideScriptRunResult] =
    Task
      .evalAsync {
        try RideRunnerStats.rideRequestRunTime.measure(evaluate(request, prevResult))
        catch {
          case e: Throwable =>
            log.error(s"An error during running $request: ${e.getMessage}")
            rideScriptFailedCalls.increment()
            fail(e match {
              case e: StatusRuntimeException if isServerError(e.getStatus) => Unknown
              case e: StatusException if isServerError(e.getStatus)        => Unknown
              case e                                                       => CustomValidationError(e.getMessage)
            })
        }
      }
      .executeOn(runScriptScheduler)

  private def isServerError(status: Status): Boolean =
    status.getCode == Status.INTERNAL.getCode || status.getCode == Status.RESOURCE_EXHAUSTED.getCode

  private def evaluate(request: RideScriptRunRequest, prevResult: RideScriptRunResult): RideScriptRunResult =
    parse(request, prevResult)
      .map { case (evaluation, scriptInfo) =>
        val initJsResult = UtilsEvaluator.evaluate(
          scriptInfo = scriptInfo,
          evaluation = evaluation,
          dAppAddress = request.address,
          options = UtilsEvaluator.EvaluateOptions(
            evaluateScriptComplexityLimit = settings.evaluateScriptComplexityLimit,
            maxTxErrorLogSize = settings.maxTxErrorLogSize,
            enableTraces = settings.enableTraces && request.trace,
            intAsString = request.intAsString
          ),
          wrapDAppEnv = underlying =>
            new TrackedDAppEnvironment(
              underlying,
              new DefaultDAppEnvironmentTracker(allTags, request)
            )
        )

        val afterStateChanges = if (settings.enableStateChanges) initJsResult else initJsResult - "stateChanges"
        val finalJsResult     = afterStateChanges ++ request.requestBody ++ Json.obj("address" -> request.address.toString)
        val finalStringResult = finalJsResult.toString()

        lazy val complexity = (finalJsResult \ "complexity").as[Int]
        val isError         = (finalJsResult \ "error").isDefined
        if (isError) {
          log.trace(s"result=failed; ${request.shortLogPrefix} errorCode=${(finalJsResult \ "error").as[Int]}")
          RideRunnerStats.rideScriptFailedCalls.increment()
        } else if (prevResult.lastResult.isEmpty || finalStringResult != prevResult.lastResult) {
          RideRunnerStats.rideScriptOkCalls.increment()
          log.trace(s"result=ok; ${request.shortLogPrefix} complexity=$complexity")
        } else {
          RideRunnerStats.rideScriptUnnecessaryCalls.increment()
          log.trace(s"result=unnecessary; ${request.shortLogPrefix} complexity=$complexity")
        }

        RideScriptRunResult(
          Some(evaluation),
          finalStringResult,
          if (isError) StatusCodes.BadRequest else StatusCodes.OK
        )
      }
      .leftMap { e =>
        val failJson = UtilsEvaluator.validationErrorToJson(e, settings.maxTxErrorLogSize)
        RideScriptRunResult(None, failJson.toString(), StatusCodes.BadRequest)
      }
      .merge

  private def parse(request: RideScriptRunRequest, prevResult: RideScriptRunResult): Either[ValidationError, (Evaluation, AccountScriptInfo)] =
    prevResult.evaluation match {
      case Some(evaluation) =>
        evaluation.blockchain.accountScript(request.address) match {
          case Some(scriptInfo) => (evaluation, scriptInfo).asRight
          case None             => GenericError(s"There is no script on '${request.address}'").asLeft
        }

      case None =>
        Evaluation.build(blockchain, request.address, request.requestBody) match {
          case Right((evaluation, scriptInfo)) => (evaluation, scriptInfo).asRight
          case Left(e)                         => e.asLeft
        }
    }

  // TODO #19 Change/move an error to an appropriate layer
  private def fail(reason: ApiError): RideScriptRunResult = RideScriptRunResult(
    evaluation = None,
    lastResult = reason.json.toString(),
    lastStatus = reason.code
  )
}

object DefaultRequestService {
  case class Settings(
      enableTraces: Boolean,
      enableStateChanges: Boolean,
      evaluateScriptComplexityLimit: Int,
      maxTxErrorLogSize: Int,
      parallelRideRunThreads: Int,
      cacheSize: ConfigMemorySize,
      cacheTtl: FiniteDuration,
      ignoredCleanupThreshold: Int
  )
}
