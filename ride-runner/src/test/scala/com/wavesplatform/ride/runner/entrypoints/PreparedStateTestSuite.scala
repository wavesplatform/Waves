package com.wavesplatform.ride.runner.entrypoints

import com.wavesplatform.api.http.utils.UtilsApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.history.DefaultBlockchainSettings
import com.wavesplatform.lang.API
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.ride.runner.blockchain.ImmutableBlockchain
import com.wavesplatform.ride.runner.input.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}
import play.api.libs.json.Json

class PreparedStateTestSuite extends BaseTestSuite with HasTestAccounts {
  private val btc  = IssuedAsset(ByteStr.decodeBase58("8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS").get)
  private val txId = ByteStr.decodeBase58("8rc5Asw43qbq7LMZ6tu2aVbVkw72XmBt7tTnwMSNfaNq").get

  private lazy val defaultInput = RideRunnerInput(
    address = scriptedAccAddr,
    request = Json.obj("expr" -> "foo(1)"),
    accounts = Map(
      scriptedAccAddr -> RunnerAccountState(
        scriptInfo = Some(RunnerScriptInfo(script = mkAccountScript(hasPayments = false)))
      ),
      aliceAddr -> RunnerAccountState(data = Some(Map("a" -> IntegerRunnerDataEntry(11)))),
      bobAddr -> RunnerAccountState(
        data = Some(Map.empty),
        balance = Map(
          Waves -> 2,
          btc   -> 1
        ),
        leasing = Some(RunnerLeaseBalance(in = 10, out = 100)),
        aliases = List("carl")
      )
    ),
    height = 3296627,
    assets = Map(
      btc -> RunnerAssetInfo(
        description = "Bitcoin",
        reissuable = true,
        quantity = 21000000,
        script = Some(assetScript)
      )
    ),
    blocks = Map(
      3296627 -> RunnerBlockInfo(
        timestamp = 1663299568885L,
        VRF = Some(ByteStr.decodeBase58("GHC3DQuW9ncm5sNy5u3TVEF4CXu1fsLVHVHYxJzuZr7b").get)
      )
    ),
    transactions = Map(
      txId -> RunnerTransactionInfo(
        amount = 93119130,
        recipient = Some(bobAddr.toString),
        timestamp = 1663299600039L,
        height = Some(3281000)
      )
    )
  )

  "PreparedState" - {
    "expr" in {
      val input      = defaultInput
      val blockchain = new ImmutableBlockchain(DefaultBlockchainSettings, input)

      val apiResult = UtilsApiRoute.evaluate(
        evaluateScriptComplexityLimit = Int.MaxValue,
        blockchain = blockchain,
        address = input.address,
        request = input.request,
        trace = input.trace,
        maxTxErrorLogSize = 0,
        intAsString = true
      )

      withClue(Json.prettyPrint(apiResult)) {
        (apiResult \ "result" \ "value" \ "_2" \ "value").as[Long] shouldBe 9007199361030957L
      }
    }

    "invoke" in {
      val input = defaultInput.copy(
        request = Json.obj(
          "dApp" -> scriptedAccAddr.toString,
          "call" -> Json.obj(
            "function" -> "foo",
            "args"     -> List(Json.obj("type" -> "integer", "value" -> 1))
          ),
          "payment" -> List(Json.obj("amount" -> 1, "assetId" -> btc.toString))
        ),
        accounts = defaultInput.accounts.updated(
          scriptedAccAddr,
          RunnerAccountState(
            scriptInfo = Some(RunnerScriptInfo(script = mkAccountScript(hasPayments = true)))
          )
        )
      )
      val blockchain = new ImmutableBlockchain(DefaultBlockchainSettings, input)

      val apiResult = UtilsApiRoute.evaluate(
        evaluateScriptComplexityLimit = Int.MaxValue,
        blockchain = blockchain,
        address = input.address,
        request = input.request,
        trace = input.trace,
        maxTxErrorLogSize = 0,
        intAsString = true
      )

      withClue(Json.prettyPrint(apiResult)) {
        (apiResult \ "result" \ "value" \ "_2" \ "value").as[Long] shouldBe 9007199361030958L
      }
    }
  }

  private def mkAccountScript(hasPayments: Boolean) = scriptFrom(s"""
{-#STDLIB_VERSION 6 #-}
{-#SCRIPT_TYPE ACCOUNT #-}
{-#CONTENT_TYPE DAPP #-}

@Callable(inv)
func foo(x: Int) = {
  let alice = Address(base58'$aliceAddr')
  let carl = addressFromRecipient(Alias("carl"))
  let bob = Address(base58'$bobAddr')

  let asset = base58'$btc'
  let txId = base58'8rc5Asw43qbq7LMZ6tu2aVbVkw72XmBt7tTnwMSNfaNq'

  # Functions
  let x1 = getIntegerValue(alice, "a")
  let x2 = 0 # if (isDataStorageUntouched(carl)) then 1 else 0 # Not supported
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
  let y3 = size(inv.payments)

  ([ScriptTransfer(bob, 1, asset)], x + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + y1 + y2 ${if (hasPayments) "+ y3" else ""} + 9007199254740991)
}

@Callable(inv)
func bar() = {
  let x1 = if (valueOrElse(getBoolean("b"), false)) then 1 else 0
  ([], x1)
}""")

  private lazy val assetScript = scriptFrom(""" 
{-# STDLIB_VERSION 5 #-}
{-# CONTENT_TYPE EXPRESSION #-}
height > 0
""")

  private lazy val estimatorV3 = ScriptEstimatorV3(fixOverflow = true, overhead = false)
  private def scriptFrom(src: String): Script =
    API
      .compile(input = src, estimatorV3)
      .flatMap(x => Script.fromBase64String(Base64.encode(x.bytes)))
      .explicitGet()
}
