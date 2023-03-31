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
import com.wavesplatform.ride.runner.storage.{RequestsStorage, ScriptRequest, SharedBlockchainStorage}
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.json.JsObject

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.concurrent.duration.FiniteDuration

trait RequestService {
  def runAll(): Task[Unit]
  def runAffected(atHeight: Int, affected: Set[ScriptRequest]): Task[Unit]
  def trackAndRun(request: ScriptRequest): Task[JsObject]
}

class DefaultRequestService(
    settings: DefaultRequestService.Settings,
    db: RideDbAccess,
    sharedBlockchain: SharedBlockchainStorage[ScriptRequest],
    requestsStorage: RequestsStorage,
    runScriptsScheduler: Scheduler
) extends RequestService
    with ScorexLogging {
  private val requests = Caffeine
    .newBuilder()
    .softValues()
    .maximumSize(10000) // TODO #96 settings and metrics
    .recordStats(() => new KamonCaffeineStats("Requests"))
    .build[ScriptRequest, RideScriptRunEnvironment]()

  // To get rid of duplicated requests
  private val inProgress = new ConcurrentHashMap[ScriptRequest, Task[JsObject]]()

  override def runAll(): Task[Unit] = {
    /*storage.asyncReadWrite { implicit ctx =>
    runManyAll(sharedBlockchain.height, scripts.values.toList)
  }*/
    Task.unit
  }

  override def runAffected(atHeight: Int, affected: Set[ScriptRequest]): Task[Unit] =
    db
      .asyncReadWrite { implicit ctx =>
        runMany(atHeight, affected.toList.flatMap(x => Option(requests.getIfPresent(x))))
      }
      .tapEval { _ =>
        Task {
          RideRunnerStats.rideRequestTotalNumber.update(requests.estimatedSize().toDouble)
          RideRunnerStats.rideRequestAffectedNumber.update(affected.size.toDouble)
        }
      }

  override def trackAndRun(request: ScriptRequest): Task[JsObject] = {
    val currentHeight = Height(sharedBlockchain.height)
    val cache         = Option(requests.getIfPresent(request))
    cache match {
      case Some(cache) =>
        rideRequestCacheHits.increment()
        Task.now(cache.lastResult)

      case _ =>
        rideRequestCacheMisses.increment()
        val task = db.asyncReadWrite { implicit ctx =>
          sharedBlockchain.accountScripts.getUntagged(currentHeight, request.address) match {
            // TODO #19 Change/move an error to an appropriate layer
            case None => Task.raiseError(ApiException(CustomValidationError(s"Address ${request.address} is not dApp")))
            case _    => runOne(currentHeight, RideScriptRunEnvironment(request), hasCaches = false)
          }
        }

        val completeTask = task.doOnFinish(_ => Task(inProgress.remove(request)))
        inProgress.computeIfAbsent(request, _ => completeTask)
    }
  }

  private def runManyAll(height: Int, scripts: Iterable[RideScriptRunEnvironment])(implicit ctx: ReadWrite): Task[Unit] = {
    val r = Task
      .parTraverseUnordered(scripts) { script =>
        runOne(height, script, hasCaches = true)
      }
      .as(())
      .executeOn(runScriptsScheduler)

    val start = RideRunnerStats.rideScriptRunOnHeightTime(true).start()
    r.tapEval(_ => Task.now(start.stop()))
  }

  private def runMany(height: Int, scripts: Iterable[RideScriptRunEnvironment])(implicit ctx: ReadWrite): Task[Unit] = {
    val r = Task
      .parTraverseUnordered(scripts)((script: RideScriptRunEnvironment) => runOne(height, script, hasCaches = true))
      .as(())
      .executeOn(runScriptsScheduler)

    val start = RideRunnerStats.rideScriptRunOnHeightTime(false).start()
    r.tapEval(_ => Task.now(start.stop()))
  }

  private def runOne(height: Int, script: RideScriptRunEnvironment, hasCaches: Boolean)(implicit ctx: ReadWrite): Task[JsObject] = Task
    .evalAsync {
      val updatedResult = rideRequestRunTime.withTag("has-caches", hasCaches).measure {
        evaluate(script) - "stateChanges" // Not required for now
      }

      val origResult = script.lastResult
      requests.put(
        script.key,
        script.copy(lastResult = updatedResult, updateHeight = height, lastUpdateInS = runScriptsScheduler.clockMonotonic(TimeUnit.SECONDS))
      )

      lazy val complexity = (updatedResult \ "complexity").as[Int]
      if ((updatedResult \ "error").isDefined) {
        log.trace(f"result=failed; addr=${script.key.address}; req=${script.key.requestBody}; errorCode=${(updatedResult \ "error").as[Int]}")
        rideScriptFailedCalls.increment()
      } else if (
        origResult == JsObject.empty ||
        updatedResult \ "result" \ "value" != origResult \ "result" \ "value"
      ) {
        rideScriptOkCalls.increment()
        log.trace(f"result=ok; addr=${script.key.address}; req=${script.key.requestBody}; complexity=$complexity")
      } else {
        rideScriptUnnecessaryCalls.increment()
        // TODO RequestKey.toString
        log.trace(f"result=unnecessary; addr=${script.key.address}; req=${script.key.requestBody}; complexity=$complexity")
      }

      updatedResult
    }
    .tapError { e => Task(log.error(s"An error during running ${script.key}", e)) }
    .executeOn(runScriptsScheduler)

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
