package com.wavesplatform.ride.runner.requests

import com.github.benmanes.caffeine.cache.Caffeine
import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.api.http.ApiException
import com.wavesplatform.api.http.utils.UtilsApiRoute
import com.wavesplatform.ride.runner.blockchain.ProxyBlockchain
import com.wavesplatform.ride.runner.db.{ReadWrite, RideDbAccess}
import com.wavesplatform.ride.runner.environments.{DefaultDAppEnvironmentTracker, TrackedDAppEnvironment}
import com.wavesplatform.ride.runner.requests.DefaultRequestService.RideScriptRunEnvironment
import com.wavesplatform.ride.runner.stats.RideRunnerStats.*
import com.wavesplatform.ride.runner.stats.{KamonCaffeineStats, RideRunnerStats}
import com.wavesplatform.ride.runner.storage.{ScriptRequest, SharedBlockchainStorage}
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.{CancelablePromise, Scheduler}
import monix.reactive.Observable
import play.api.libs.json.JsObject

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait RequestService extends AutoCloseable {
  def start(): Unit
  def runAll(): Task[Unit]
  // TODO prioritizeAffected
  def runAffected(atHeight: Int, affected: Set[ScriptRequest]): Task[Unit]
  def trackAndRun(request: ScriptRequest): Task[JsObject]
}

class RequestJob private (workers: Int, val result: CancelablePromise[JsObject]) {
  def inProgress: Boolean   = workers > 0
  def isAvailable: Boolean  = workers <= 1
  def proceed(): RequestJob = new RequestJob(workers + 1, result)
}

object RequestJob {
  def mk(): RequestJob = new RequestJob(workers = 0, CancelablePromise[JsObject]())
}

class DefaultRequestService(
    settings: DefaultRequestService.Settings,
    db: RideDbAccess,
    sharedBlockchain: SharedBlockchainStorage[ScriptRequest],
    requestScheduler: JobScheduler[ScriptRequest],
    runScriptScheduler: Scheduler
) extends RequestService
    with ScorexLogging {
  private val isWorking = new AtomicBoolean(true)

  // Promise to get rid of duplicated requests
  // TODO limit in time?
  private val requests = Caffeine
    .newBuilder()
    .maximumSize(10000) // TODO #96 settings and metrics
    .recordStats(() => new KamonCaffeineStats("Requests"))
    .build[ScriptRequest, RideScriptRunEnvironment]()

  // TODO curr jobs, RequestState -> RequestJob?
  private val currJobs = new ConcurrentHashMap[ScriptRequest, RequestJob]()

  override def start(): Unit = {
    val task =
      if (!isWorking.get()) Task.unit
      else
        Observable
          .repeatEval(requestScheduler.getJob())
          .takeWhile(_ => isWorking.get())
          .concatMap {
            case Some(x) => Observable.now(x)
            case None =>
              Observable
                .fromTask(Task.sleep(100.millis))
                .flatMap(_ => Observable.empty)
          }
          .mapParallelUnordered(3) { toRun =>
            val orig = currJobs.computeIfAbsent(toRun, _ => RequestJob.mk())
            if (orig.inProgress) Task.unit
            else
              Option(currJobs.computeIfPresent(toRun, (_, x) => x.proceed())) match {
                case Some(updated) =>
                  if (updated.isAvailable) {
                    // We don't need to cache an empty value, so we use getOrElse here
                    val origEnv = Option(requests.getIfPresent(toRun)).getOrElse(RideScriptRunEnvironment(toRun))
                    db.asyncReadWrite { implicit ctx =>
                      runOne(sharedBlockchain.heightUntagged, origEnv, hasCaches = true).tapEval { r =>
                        Task {
                          requests.put(r.key, r)
                          updated.result.trySuccess(r.lastResult)
                          currJobs.remove(toRun)
                        }
                      }
                    }
                  } else {
                    log.info(s"$toRun is already running")
                    // A job is processed now, but it can use stale data
                    Task(requestScheduler.add(toRun))
                  }

                case _ => Task.unit
              }
          }
          .lastL
          .as(())

    // TODO cancel?
    task.runToFuture(runScriptScheduler)
  }

  override def close(): Unit = {
    isWorking.set(false)
//    Await.result(currTask)
  }

  override def runAll(): Task[Unit] = {
    /*storage.asyncReadWrite { implicit ctx =>
    runManyAll(sharedBlockchain.height, scripts.values.toList)
  }*/
    Task.unit
  }

  override def runAffected(reason: Int, affected: Set[ScriptRequest]): Task[Unit] = Task {
    requestScheduler.addMultiple(affected)
    RideRunnerStats.rideRequestTotalNumber.update(requests.estimatedSize().toDouble)
  }

  override def trackAndRun(request: ScriptRequest): Task[JsObject] = {
    val currentHeight = Height(sharedBlockchain.heightUntagged)
    val cache         = Option(requests.getIfPresent(request))
    cache match {
      case Some(cache) =>
        rideRequestCacheHits.increment()
        Task.now(cache.lastResult)

      case _ =>
        rideRequestCacheMisses.increment()
        Task {
          db.readWrite { implicit ctx =>
            sharedBlockchain.accountScripts.getUntagged(currentHeight, request.address)
          }
        }.flatMap {
          // TODO #19 Change/move an error to an appropriate layer
          case None => Task.raiseError(ApiException(CustomValidationError(s"Address ${request.address} is not dApp")))
          case _ =>
            log.info(s"Adding ${request.logPrefix} as $request")
            val job = RequestJob.mk()
            currJobs.putIfAbsent(request, job)
            requestScheduler.add(request)
            Task.fromCancelablePromise(job.result)
        }
    }
  }

  private def runManyAll(height: Int, scripts: Iterable[RideScriptRunEnvironment])(implicit ctx: ReadWrite): Task[Unit] = {
//    val r = Task
//      .parTraverseUnordered(scripts) { script =>
//        runOne(height, script, hasCaches = true)
//      }
//      .as(())
//      .executeOn(runScriptScheduler)
//
//    val start = RideRunnerStats.rideScriptRunOnHeightTime(true).start()
//    r.tapEval(_ => Task.now(start.stop()))
    Task.unit
  }

  private def runMany(height: Int, scripts: Iterable[RideScriptRunEnvironment])(implicit ctx: ReadWrite): Task[Unit] = {
//    val r = Task
//      .parTraverseUnordered(scripts)((script: RideScriptRunEnvironment) => runOne(height, script, hasCaches = true))
//      .as(())
//      .executeOn(runScriptScheduler)
//
//    val start = RideRunnerStats.rideScriptRunOnHeightTime(false).start()
//    r.tapEval(_ => Task.now(start.stop()))
    Task.unit
  }

  private def runOne(height: Int, scriptEnv: RideScriptRunEnvironment, hasCaches: Boolean)(implicit ctx: ReadWrite): Task[RideScriptRunEnvironment] =
    Task
      .evalAsync {
        val updatedResult = rideRequestRunTime.withTag("has-caches", hasCaches).measure {
          evaluate(scriptEnv) - "stateChanges" // Not required for now
        }

        val origResult      = scriptEnv.lastResult
        lazy val complexity = (updatedResult \ "complexity").as[Int]
        if ((updatedResult \ "error").isDefined) {
          log.trace(f"result=failed; ${scriptEnv.key} errorCode=${(updatedResult \ "error").as[Int]}")
          rideScriptFailedCalls.increment()
        } else if (
          origResult == JsObject.empty ||
          updatedResult \ "result" \ "value" != origResult \ "result" \ "value"
        ) {
          rideScriptOkCalls.increment()
          log.trace(f"result=ok; ${scriptEnv.key} complexity=$complexity")
        } else {
          rideScriptUnnecessaryCalls.increment()
          log.trace(f"result=unnecessary; ${scriptEnv.key} complexity=$complexity")
        }

        scriptEnv.copy(
          lastResult = updatedResult,
          updateHeight = height,
          lastUpdateInS = runScriptScheduler.clockMonotonic(TimeUnit.SECONDS)
        )
      }
      .tapError { e => Task(log.error(s"An error during running ${scriptEnv.key}", e)) }
      .executeOn(runScriptScheduler)

  private def evaluate(script: RideScriptRunEnvironment)(implicit ctx: ReadWrite): JsObject = UtilsApiRoute.evaluate(
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

  private final case class RideScriptRunEnvironment(
      key: ScriptRequest,
      lastResult: JsObject,
      updateHeight: Int,
      lastUpdateInS: Long
  )

  private object RideScriptRunEnvironment {
    def apply(key: ScriptRequest): RideScriptRunEnvironment = new RideScriptRunEnvironment(key, JsObject.empty, 0, 0L)
  }
}
