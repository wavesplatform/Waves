package com.wavesplatform.ride

import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.api.http.ApiException
import com.wavesplatform.api.http.utils.UtilsApiRoute
import com.wavesplatform.blockchain.{ScriptBlockchain, SharedBlockchainData}
import com.wavesplatform.ride.DefaultRequestsService.RideScriptRunEnvironment
import com.wavesplatform.ride.app.RideRunnerMetrics
import com.wavesplatform.ride.app.RideRunnerMetrics.*
import com.wavesplatform.state.Blockchain
import com.wavesplatform.storage.{RequestKey, RequestsStorage}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.json.JsObject

import java.util.concurrent.ConcurrentHashMap
import scala.collection.concurrent.TrieMap

trait RequestsService {
  def runAll(): Task[Unit]
  // Either temporarily
  def runAffected(atHeight: Int, affected: Either[Unit, Set[RequestKey]]): Task[Unit]
  def trackAndRun(request: RequestKey): Task[JsObject]
}

class DefaultRequestsService(
    settings: DefaultRequestsService.Settings,
    sharedBlockchainData: SharedBlockchainData[RequestKey],
    storage: RequestsStorage,
    runScriptsScheduler: Scheduler
) extends RequestsService
    with ScorexLogging {
  private val scripts: TrieMap[RequestKey, RideScriptRunEnvironment] =
    TrieMap.from(storage.all().map(k => (k, RideScriptRunEnvironment(k, sharedBlockchainData))))

  // To get rid of duplicated requests
  private val inProgress = new ConcurrentHashMap[RequestKey, Task[JsObject]]()

  private val probablyNotDependentOnHeight = ConcurrentHashMap.newKeySet[RequestKey]()

  override def runAll(): Task[Unit] =
    runManyAll(sharedBlockchainData.height, scripts.values.toList.filter { v => !probablyNotDependentOnHeight.contains(v.key) })

  override def runAffected(atHeight: Int, affected: Either[Unit, Set[RequestKey]]): Task[Unit] = affected match {
    case Right(affected) => runMany(atHeight, affected.flatMap(scripts.get).toList)
    case _               => runAll()
  }

  override def trackAndRun(request: RequestKey): Task[JsObject] = {
    val height        = sharedBlockchainData.height
    val probablyStale = probablyNotDependentOnHeight.contains(request)
    scripts.get(request) match {
      case Some(r) if !probablyStale && height <= r.updateHeight =>
        rideScriptCacheHits.increment()
        Task.now(r.lastResult)

      case _ =>
        val runOneTask = runOne(height, RideScriptRunEnvironment(request, sharedBlockchainData), hasCaches = false)
        val task =
          if (probablyStale) runOneTask
          else
            Task {
              rideScriptCacheMisses.increment()
              storage.append(request)
              sharedBlockchainData.accountScripts.getUntagged(height, request.address)
            }.flatMap {
              // TODO #19 Change/move an error to an appropriate layer
              case None => Task.raiseError(ApiException(CustomValidationError(s"Address ${request.address} is not dApp")))
              case _    => runOneTask
            }

        val completeTask = task.doOnFinish(_ => Task(inProgress.remove(request)))
        inProgress.computeIfAbsent(request, _ => completeTask)
    }
  }

  private def runManyAll(height: Int, scripts: Iterable[RideScriptRunEnvironment]): Task[Unit] = {
    val r = Task
      .parTraverseUnordered(scripts) { script =>
        val origResult = script.lastResult
        runOne(height, script, hasCaches = true).tapEval { updatedResult =>
          Task {
            if (!(origResult == JsObject.empty || updatedResult \ "result" \ "value" != origResult \ "result" \ "value"))
              probablyNotDependentOnHeight.add(script.key)
          }
        }
      }
      .as(())
      .executeOn(runScriptsScheduler)

    val start = System.nanoTime()
    r.tapEval(_ => Task.now(RideRunnerMetrics.rideScriptRunOnHeightTime(true).update((System.nanoTime() - start).toDouble)))
  }

  private def runMany(height: Int, scripts: Iterable[RideScriptRunEnvironment]): Task[Unit] = {
    val r = Task
      .parTraverseUnordered(scripts)((script: RideScriptRunEnvironment) => runOne(height, script, hasCaches = true))
      .as(())
      .executeOn(runScriptsScheduler)

    val start = System.nanoTime()
    r.tapEval(_ => Task.now(RideRunnerMetrics.rideScriptRunOnHeightTime(false).update((System.nanoTime() - start).toDouble)))
  }

  private def runOne(height: Int, script: RideScriptRunEnvironment, hasCaches: Boolean): Task[JsObject] = Task {
    val updatedResult = rideScriptRunTime.withTag("has-caches", hasCaches).measure {
      val result = evaluate(script)
      // Removed, because I'm not sure it is required. TODO restore stateChanges?
      result - "stateChanges"
    }

    val origResult = script.lastResult
    scripts.put(
      script.key,
      script.copy(lastResult = updatedResult, updateHeight = height)
    )

    if ((updatedResult \ "error").isEmpty) {
      val complexity = (updatedResult \ "complexity").as[Int]
      log.info(f"result=ok; addr=${script.key.address}; req=${script.key.requestBody}; complexity=$complexity")

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

  private def evaluate(script: RideScriptRunEnvironment): JsObject = UtilsApiRoute.evaluate(
    settings.evaluateScriptComplexityLimit,
    script.blockchain,
    script.key.address,
    script.key.requestBody,
    settings.enableTraces,
    settings.maxTxErrorLogSize
  )
}

object DefaultRequestsService {
  case class Settings(enableTraces: Boolean, evaluateScriptComplexityLimit: Int, maxTxErrorLogSize: Int)

  private final case class RideScriptRunEnvironment(
      blockchain: Blockchain,
      key: RequestKey,
      lastResult: JsObject,
      updateHeight: Int
  )

  private object RideScriptRunEnvironment {
    def apply(key: RequestKey, blockchainStorage: SharedBlockchainData[RequestKey]): RideScriptRunEnvironment =
      new RideScriptRunEnvironment(new ScriptBlockchain[RequestKey](blockchainStorage, key), key, JsObject.empty, 0)
  }
}
