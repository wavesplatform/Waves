package com.wavesplatform

import com.wavesplatform.api.http.ApiError
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.{API, ValidationError}
import com.wavesplatform.ride.input.RunnerRequest
import com.wavesplatform.serialization.ScriptValuesJson
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.transaction.TxValidationError.{GenericError, ScriptExecutionError}
import com.wavesplatform.transaction.smart.script.trace.{InvokeScriptTrace, TraceStep}
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsObject, Json}

package object ride {
  val exampleScriptSrc =
    """
{-#STDLIB_VERSION 6 #-}
{-#SCRIPT_TYPE ACCOUNT #-}
{-#CONTENT_TYPE DAPP #-}

@Callable(inv)
func foo(x: Int) = {
let alice = Address(base58'3P6GhtTsABtYUgzhXTA4cDwbqqy7HqruiQQ')
let carl = addressFromRecipient(Alias("carl"))
let bob = Address(base58'3PE7TH41wVuhn2SpAwWBBzeGxxzz8wXrb6L')
let jane = Address(base58'3P4xDBqzXgR8HyXoyNn1C8Bd88h4rsEBMHA')

let asset = base58'8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS'
let txId = base58'8rc5Asw43qbq7LMZ6tu2aVbVkw72XmBt7tTnwMSNfaNq'

# Functions
let x1 = getIntegerValue(alice, "a")
let x2 = if (isDataStorageUntouched(carl)) then 1 else 0
let x3 = assetBalance(bob, asset)
let x4 = value(assetInfo(asset)).decimals
let x5 = value(blockInfoByHeight(3296627)).height
let x6 = size(value(scriptHash(this)))
let x7 = value(transactionHeightById(txId))
let x8 = value(transferTransactionById(txId)).amount
let x9 = wavesBalance(carl).available
let x10 = invoke(this, "bar", [], []).exactAs[Int]

# Vals
let y1 = height
let y2 = lastBlock.height

([ScriptTransfer(bob, 1, asset)], x + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + y1 + y2 + 9007199254740991)
}

@Callable(inv)
func bar() = {
let x1 = if (valueOrElse(getBoolean("b"), false)) then 1 else 0
([], x1)
}"""
  val estimatorV3    = ScriptEstimatorV3(fixOverflow = true, overhead = false)
  val compiledScript = API.compile(input = exampleScriptSrc, estimatorV3).explicitGet()

  def execute(
      blockchain: Blockchain,
      request: RunnerRequest
  ): JsObject = {
    val log = LoggerFactory.getLogger("execute")
    val tx  = request.toTx(blockchain.settings.addressSchemeCharacter.toByte)
//    log.info("Transaction: {}", tx)
//    log.info("Running from {}", tx.sender.toAddress)

    val result = InvokeScriptTransactionDiff(
      blockchain,
      System.currentTimeMillis(), // blockTime
      limitedExecution = false
    )(tx)

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
