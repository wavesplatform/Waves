package com.wavesplatform.ride.runner.entrypoints

import com.wavesplatform.account.Alias
import com.wavesplatform.api.http.utils.UtilsEvaluator
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.history.DefaultBlockchainSettings
import com.wavesplatform.ride.runner.TestScript
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
        scriptInfo = Some(RunnerScriptInfo(script = mkAccountScript(hasPayments = false))),
        balance = Map(
          Waves -> (1_300_000 + 2),
          btc   -> 5
        )
      ),
      aliceAddr -> RunnerAccountState(
        data = Some(Map("a" -> IntegerRunnerDataEntry(11))),
        aliases = List(Alias.create("carl").explicitGet())
     ),
      bobAddr -> RunnerAccountState(
        data = Some(Map.empty),
        balance = Map(
          Waves -> 2,
          btc   -> 1
        ),
        leasing = Some(RunnerLeaseBalance(in = 10, out = 100))
      )
    ),
    height = 3296627,
    assets = Map(
      btc -> RunnerAssetInfo(
        description = "Bitcoin",
        reissuable = true,
        quantity = 21000000
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

      val apiResult = UtilsEvaluator.evaluate(
        evaluateScriptComplexityLimit = Int.MaxValue,
        blockchain = blockchain,
        address = input.address,
        request = input.request,
        trace = input.trace,
        maxTxErrorLogSize = 0,
        intAsString = true
      )

      withClue(Json.prettyPrint(apiResult)) {
        (apiResult \ "result" \ "value" \ "_2" \ "value").as[BigInt] shouldBe 9007199361031055L
      }
    }

    "invoke" in {
      val input = defaultInput.copy(
        request = Json.obj( // See UtilsInvocationRequest
          "call" -> Json.obj(
            "function" -> "foo",
            "args"     -> List(Json.obj("type" -> "integer", "value" -> 1))
          ),
          "sender"          -> scriptedAccAddr.toString,
          "senderPublicKey" -> scriptedAcc.publicKey.toString,
          "payment"         -> List(Json.obj("amount" -> 1, "assetId" -> btc.toString)),
          "fee"             -> 1_300_000
        ),
        accounts = defaultInput.accounts.updated(
          scriptedAccAddr,
          RunnerAccountState(
            scriptInfo = Some(
              RunnerScriptInfo(
                script = mkAccountScript(hasPayments = true),
                publicKey = scriptedAcc.publicKey
              )
            ),
            balance = Map(
              Waves -> (5 * 1_700_000 + 2),
              btc   -> 5
            )
          )
        )
      )
      val blockchain = new ImmutableBlockchain(DefaultBlockchainSettings, input)

      val apiResult = UtilsEvaluator.evaluate(
        evaluateScriptComplexityLimit = Int.MaxValue,
        blockchain = blockchain,
        address = input.address,
        request = input.request,
        trace = input.trace,
        maxTxErrorLogSize = 0,
        intAsString = true
      )

      withClue(Json.prettyPrint(apiResult)) {
        (apiResult \ "result" \ "value" \ "_2" \ "value").as[BigInt] shouldBe 9007199361031056L
      }
    }
  }

  private def mkAccountScript(hasPayments: Boolean) = TestScript.scriptFrom(s"""
{-#STDLIB_VERSION 6 #-}
{-#SCRIPT_TYPE ACCOUNT #-}
{-#CONTENT_TYPE DAPP #-}

@Callable(inv)
func foo(x: Int) = {
  let alice = Address(base58'$aliceAddr')
  let carl = addressFromRecipient(Alias("carl"))
  let bob = Address(base58'$bobAddr')

  let asset = base58'$btc'
  let txId = base58'$txId'

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

  ([ScriptTransfer(bob, 1, asset)], x + x1 + x2 + x3 + x4 + x5 +  x6 + x7     + x8    + x9 + x10 + y1 + y2 ${if (hasPayments) "+ y3" else ""} + 9007199254740991)
}

@Callable(inv)
func bar() = {
  let x1 = if (valueOrElse(getBoolean("b"), false)) then 1 else 0
  ([], x1)
}""")

}