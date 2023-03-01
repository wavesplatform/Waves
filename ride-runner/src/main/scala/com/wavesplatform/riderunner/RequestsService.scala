package com.wavesplatform.riderunner

import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.api.http.ApiException
import com.wavesplatform.api.http.utils.UtilsApiRoute
import com.wavesplatform.blockchain.ScriptBlockchain
import com.wavesplatform.riderunner.DefaultRequestsService.RideScriptRunEnvironment
import com.wavesplatform.riderunner.app.RideRunnerMetrics
import com.wavesplatform.riderunner.app.RideRunnerMetrics.*
import com.wavesplatform.riderunner.storage.StorageContext.ReadWrite
import com.wavesplatform.riderunner.storage.{RequestKey, RequestsStorage, SharedBlockchainStorage, Storage}
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.json.JsObject

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.FiniteDuration

trait RequestsService {
  def runAll(): Task[Unit]
  def runAffected(atHeight: Int, affected: Set[RequestKey]): Task[Unit]
  def trackAndRun(request: RequestKey): Task[JsObject]
}

class DefaultRequestsService(
    settings: DefaultRequestsService.Settings,
    storage: Storage,
    sharedBlockchain: SharedBlockchainStorage[RequestKey],
    requestsStorage: RequestsStorage,
    runScriptsScheduler: Scheduler
) extends RequestsService
    with ScorexLogging {
  private val scripts: TrieMap[RequestKey, RideScriptRunEnvironment] =
    TrieMap.from(requestsStorage.all().map(k => (k, RideScriptRunEnvironment(k))))

//  private val jobScheduler: JobScheduler[RequestKey] = SynchronizedJobScheduler(settings.parallelization, scripts.keys)

  // To get rid of duplicated requests
  private val inProgress = new ConcurrentHashMap[RequestKey, Task[JsObject]]()

  override def runAll(): Task[Unit] = storage.asyncReadWrite { implicit ctx =>
    runManyAll(sharedBlockchain.height, scripts.values.toList)
  }

  override def runAffected(atHeight: Int, affected: Set[RequestKey]): Task[Unit] =
    // runMany(atHeight, affected.toList.flatMap(scripts.get))
    // Task.now(jobScheduler.prioritize(affected))
    Task.unit

  override def trackAndRun(request: RequestKey): Task[JsObject] = {
    val currentHeight = Height(sharedBlockchain.height)
    val cache         = scripts.get(request)
    cache match {
      // TODO -1 ? Will eliminate calls to blockchain
      case Some(cache) if runScriptsScheduler.clockMonotonic(TimeUnit.SECONDS) - cache.lastUpdateInS < settings.cacheTtl.toSeconds =>
        // if currentHeight <= cache.updateHeight =>
        rideScriptCacheHits.increment()
        Task.now(cache.lastResult)

      case _ =>
        val task = storage.asyncReadWrite { implicit rw =>
          Task {
            rideScriptCacheMisses.increment()
            if (cache.isEmpty) requestsStorage.append(request)
            sharedBlockchain.accountScripts.getUntagged(currentHeight, request.address)
          }.flatMap {
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

    val start = System.nanoTime()
    r.tapEval(_ => Task.now(RideRunnerMetrics.rideScriptRunOnHeightTime(true).update((System.nanoTime() - start).toDouble)))
  }

  private def runMany(height: Int, scripts: Iterable[RideScriptRunEnvironment])(implicit ctx: ReadWrite): Task[Unit] = {
    val r = Task
      .parTraverseUnordered(scripts)((script: RideScriptRunEnvironment) => runOne(height, script, hasCaches = true))
      .as(())
      .executeOn(runScriptsScheduler)

    val start = System.nanoTime()
    r.tapEval(_ => Task.now(RideRunnerMetrics.rideScriptRunOnHeightTime(false).update((System.nanoTime() - start).toDouble)))
  }

  private def runOne(height: Int, script: RideScriptRunEnvironment, hasCaches: Boolean)(implicit ctx: ReadWrite): Task[JsObject] = Task {
    val updatedResult = rideScriptRunTime.withTag("has-caches", hasCaches).measure {
      val result = evaluate(script)
      // Removed, because I'm not sure it is required. TODO restore stateChanges?
      result - "stateChanges"
    }

    val origResult = script.lastResult
    scripts.put(
      script.key,
      script.copy(lastResult = updatedResult, updateHeight = height, lastUpdateInS = runScriptsScheduler.clockMonotonic(TimeUnit.SECONDS))
    )

    if ((updatedResult \ "error").isEmpty) {
      val complexity = (updatedResult \ "complexity").as[Int]
      log.info(f"result=ok; addr=${script.key.address}; req=${script.key.requestBody}; complexity=$complexity")

      // TODO consumes CPU?
      if (
        origResult == JsObject.empty ||
        updatedResult \ "result" \ "value" != origResult \ "result" \ "value"
      ) rideScriptOkCalls.increment()
      else rideScriptUnnecessaryCalls.increment()
    } else {
      log.info(f"result=failed; addr=${script.key.address}; req=${script.key.requestBody}; errorCode=${(updatedResult \ "error").as[Int]}")
      rideScriptFailedCalls.increment()
    }

    updatedResult
  }
    .tapError { e => Task(log.error(s"An error during running ${script.key}", e)) }
    .executeOn(runScriptsScheduler)

  private def evaluate(script: RideScriptRunEnvironment)(implicit ctx: ReadWrite): JsObject = UtilsApiRoute.evaluate(
    settings.evaluateScriptComplexityLimit,
    new ScriptBlockchain[RequestKey](sharedBlockchain, script.key),
    script.key.address,
    script.key.requestBody,
    settings.enableTraces,
    settings.maxTxErrorLogSize
  )
}

object DefaultRequestsService {
  case class Settings(
      enableTraces: Boolean,
      evaluateScriptComplexityLimit: Int,
      maxTxErrorLogSize: Int,
      parallelization: Int,
      cacheTtl: FiniteDuration
  )

  private final case class RideScriptRunEnvironment(
      key: RequestKey,
      lastResult: JsObject,
      updateHeight: Int,
      lastUpdateInS: Long
  )

  private object RideScriptRunEnvironment {
    def apply(key: RequestKey): RideScriptRunEnvironment = new RideScriptRunEnvironment(key, JsObject.empty, 0, 0L)
  }
}
