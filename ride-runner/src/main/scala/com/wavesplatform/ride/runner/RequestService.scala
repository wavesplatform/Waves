package com.wavesplatform.ride.runner

import com.github.benmanes.caffeine.cache.Caffeine
import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.api.http.ApiException
import com.wavesplatform.api.http.utils.UtilsApiRoute
import com.wavesplatform.ride.runner.DefaultRequestService.RideScriptRunEnvironment
import com.wavesplatform.ride.runner.blockchain.ScriptBlockchain
import com.wavesplatform.ride.runner.stats.{KamonCaffeineStats, RideRunnerStats}
import com.wavesplatform.ride.runner.stats.RideRunnerStats.*
import com.wavesplatform.ride.runner.storage.StorageContext.ReadWrite
import com.wavesplatform.ride.runner.storage.{RequestsStorage, ScriptRequest, SharedBlockchainStorage, Storage}
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
    storage: Storage,
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
    // TODO #98 Resources in bracket
    /*storage.asyncReadWrite { implicit ctx =>
    runManyAll(sharedBlockchain.height, scripts.values.toList)
  }*/
    Task.unit
  }

  override def runAffected(atHeight: Int, affected: Set[ScriptRequest]): Task[Unit] =
    // runMany(atHeight, affected.toList.flatMap(scripts.get))
    // Task.now(jobScheduler.prioritize(affected))
    Task.unit

  override def trackAndRun(request: ScriptRequest): Task[JsObject] = {
    val currentHeight = Height(sharedBlockchain.height)
    val cache         = Option(requests.getIfPresent(request))
    cache match {
      case Some(cache) if runScriptsScheduler.clockMonotonic(TimeUnit.SECONDS) - cache.lastUpdateInS < settings.cacheTtl.toSeconds =>
        rideRequestCacheHits.increment()
        Task.now(cache.lastResult)

      case _ =>
        // TODO #98 Resources in bracket
        val task = // storage.asyncReadWrite { implicit rw =>
          Task {
            rideRequestCacheMisses.increment()
            storage.readWrite { implicit ctx =>
              sharedBlockchain.accountScripts.getUntagged(currentHeight, request.address)
            }
          }.flatMap {
            // TODO #19 Change/move an error to an appropriate layer
            case None => Task.raiseError(ApiException(CustomValidationError(s"Address ${request.address} is not dApp")))
            case _    =>
              // We don't need this for now
              // if (cache.isEmpty) requestsStorage.append(request) // TODO duplicates!!!!
              runOne(currentHeight, RideScriptRunEnvironment(request), hasCaches = false)
          }
//        }

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

    val start = System.nanoTime()
    r.tapEval(_ => Task.now(RideRunnerStats.rideScriptRunOnHeightTime(true).update((System.nanoTime() - start).toDouble)))
  }

  private def runMany(height: Int, scripts: Iterable[RideScriptRunEnvironment])(implicit ctx: ReadWrite): Task[Unit] = {
//    val r = Task
//      .parTraverseUnordered(scripts)((script: RideScriptRunEnvironment) => runOne(height, script, hasCaches = true))
//      .as(())
//      .executeOn(runScriptsScheduler)
//
//    val start = System.nanoTime()
//    r.tapEval(_ => Task.now(RideRunnerMetrics.rideScriptRunOnHeightTime(false).update((System.nanoTime() - start).toDouble)))
    Task.unit
  }

  // TODO #98 Resources in bracket
  //  private def runOne(height: Int, script: RideScriptRunEnvironment, hasCaches: Boolean)(implicit ctx: ReadWrite): Task[JsObject] = Task {
  private def runOne(height: Int, script: RideScriptRunEnvironment, hasCaches: Boolean): Task[JsObject] = Task {
    val updatedResult = storage.readWrite { implicit ctx =>
      rideRequestRunTime.withTag("has-caches", hasCaches).measure {
        evaluate(script) - "stateChanges" // Not required for now
      }
    }

    val origResult = script.lastResult
    requests.put(
      script.key,
      script.copy(lastResult = updatedResult, updateHeight = height, lastUpdateInS = runScriptsScheduler.clockMonotonic(TimeUnit.SECONDS))
    )

    if ((updatedResult \ "error").isEmpty) {
      val complexity = (updatedResult \ "complexity").as[Int]
      log.trace(f"result=ok; addr=${script.key.address}; req=${script.key.requestBody}; complexity=$complexity")

      if (
        origResult == JsObject.empty ||
        updatedResult \ "result" \ "value" != origResult \ "result" \ "value"
      ) rideScriptOkCalls.increment()
      else rideScriptUnnecessaryCalls.increment()
    } else {
      log.trace(f"result=failed; addr=${script.key.address}; req=${script.key.requestBody}; errorCode=${(updatedResult \ "error").as[Int]}")
      rideScriptFailedCalls.increment()
    }

    updatedResult
  }
    .tapError { e => Task(log.error(s"An error during running ${script.key}", e)) }
    .executeOn(runScriptsScheduler)

  private def evaluate(script: RideScriptRunEnvironment)(implicit ctx: ReadWrite): JsObject = UtilsApiRoute.evaluate(
    settings.evaluateScriptComplexityLimit,
    new ScriptBlockchain[ScriptRequest](sharedBlockchain, script.key),
    script.key.address,
    script.key.requestBody,
    settings.enableTraces,
    settings.maxTxErrorLogSize
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
