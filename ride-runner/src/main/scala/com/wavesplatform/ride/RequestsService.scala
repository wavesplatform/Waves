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

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.collection.concurrent.TrieMap

trait RequestsService {
  def runAll(): Task[Unit]
  def runAffected(atHeight: Int, affected: Set[RequestKey]): Task[Unit]
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

  override def runAll(): Task[Unit] = runMany(scripts.values, force = true)

  override def runAffected(atHeight: Int, affected: Set[RequestKey]): Task[Unit] =
    runMany(affected.flatMap(scripts.get).to(List), force = false)

  override def trackAndRun(request: RequestKey): Task[JsObject] = scripts.get(request) match {
    case Some(r) =>
      rideScriptCacheHits.increment()
      Task.now(r.lastResult)

    case None =>
      inProgress.computeIfAbsent(
        request,
        { request =>
          Task {
            rideScriptCacheMisses.increment()
            storage.append(request)
            sharedBlockchainData.accountScripts.getUntagged(sharedBlockchainData.height, request.address)
          }
            .flatMap {
              // TODO #19 Change/move an error to an appropriate layer
              case None => Task.raiseError(ApiException(CustomValidationError(s"Address ${request.address} is not dApp")))
              case _    => runOne(RideScriptRunEnvironment(request, sharedBlockchainData), hasCaches = false)
            }
            .doOnFinish { _ => Task(inProgress.remove(request)) }
        }
      )
  }

  private def runMany(scripts: Iterable[RideScriptRunEnvironment], force: Boolean): Task[Unit] = {
    val r = Task
      .parTraverseUnordered(scripts)(runOne(_, hasCaches = true))
      .as(())
      .executeOn(runScriptsScheduler)

    val start = System.nanoTime()
    r.tapEval(_ => Task.now(RideRunnerMetrics.rideScriptRunOnHeightTime(force).update((System.nanoTime() - start).toDouble)))
  }

  private def runOne(script: RideScriptRunEnvironment, hasCaches: Boolean): Task[JsObject] = Task {
    val origResult = script.lastResult

    val updatedResult = rideScriptRunTime.withTag("has-caches", hasCaches).measure {
      val result = UtilsApiRoute.evaluate(
        settings.evaluateScriptComplexityLimit,
        script.blockchain,
        script.key.address,
        script.key.requestBody,
        settings.enableTraces,
        settings.maxTxErrorLogSize
      )

      // Removed, because I'm not sure it is required. TODO restore stateChanges?
      result - "stateChanges"
    }

    scripts.put(
      script.key,
      script.copy(lastResult = updatedResult, lastUpdatedTs = runScriptsScheduler.clockMonotonic(TimeUnit.MILLISECONDS))
    )

    if ((updatedResult \ "error").isEmpty) {
      val complexity = updatedResult.value("complexity").as[Int]
      val result     = updatedResult.value("result").as[JsObject].value("value")
      log.info(f"result=ok; addr=${script.key.address}; req=${script.key.requestBody}; complexity=$complexity")

      origResult.value.get("result").map(_.as[JsObject].value("value")).foreach { prevResult =>
        if (result == prevResult) rideScriptUnnecessaryCalls.increment()
        else rideScriptOkCalls.increment()
      }
    } else {
      log.info(f"result=failed; addr=${script.key.address}; req=${script.key.requestBody}; errorCode=${(updatedResult \ "error").as[Int]}")
      rideScriptFailedCalls.increment()
    }

    updatedResult
  }
    .tapError { e => Task(log.error(s"An error during running ${script.key}", e)) }
    .executeOn(runScriptsScheduler)
}

object DefaultRequestsService {
  case class Settings(enableTraces: Boolean, evaluateScriptComplexityLimit: Int, maxTxErrorLogSize: Int)

  private final case class RideScriptRunEnvironment(blockchain: Blockchain, key: RequestKey, lastResult: JsObject, lastUpdatedTs: Long)
  private object RideScriptRunEnvironment {
    def apply(key: RequestKey, blockchainStorage: SharedBlockchainData[RequestKey]): RideScriptRunEnvironment =
      new RideScriptRunEnvironment(new ScriptBlockchain[RequestKey](blockchainStorage, key), key, JsObject.empty, 0)
  }
}
