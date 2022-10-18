package com.wavesplatform.ride

import com.wavesplatform.Application
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.ride.input.RunnerRequest
import com.wavesplatform.serialization.ScriptValuesJson
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.transaction.TxValidationError.{GenericError, ScriptExecutionError}
import com.wavesplatform.transaction.smart.script.trace.{InvokeScriptTrace, TraceStep}
import play.api.libs.json.{JsObject, Json}

import java.io.File

object RideBlockchainRunner {
  def main(args: Array[String]): Unit = {
    val basePath     = args(0)
    val nodeSettings = Application.loadApplicationConfig(Some(new File(s"$basePath/node/waves.conf")))

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = 'W'.toByte
    }

    //val mutableBlockchain = new MutableBlockchain(nodeSettings.blockchainSettings)
    //mutableBlockchain.putHasData()

//    val apiResult = execute(blockchain, input.request)
//    println(s"apiResult: $apiResult")
  }

  def execute(
      blockchain: Blockchain,
      request: RunnerRequest
  ): JsObject = {
    val result = InvokeScriptTransactionDiff(
      blockchain,
      System.currentTimeMillis(), // blockTime
      limitedExecution = false
    )(request.toTx(blockchain.settings.addressSchemeCharacter.toByte))
    result.resultE
      .flatMap { all =>
        result.trace
          .collectFirst { case trace: InvokeScriptTrace =>
            trace.resultE.map { x =>
              val base = Json.obj(
                "result"     -> ScriptValuesJson.serializeValue(x.returnedValue),
                "complexity" -> all.scriptsComplexity
              )

              if (request.trace) base ++ Json.obj(TraceStep.logJson(trace.log)) else base
            }
          }
          .toRight(GenericError("No results"): ValidationError)
          .flatten
      }
      .left
      .map {
        case e: ScriptExecutionError => Json.obj("error" -> ApiError.ScriptExecutionError.Id, "message" -> e.error)
        case e                       => ApiError.fromValidationError(e).json
      }
      .merge
  }
}
