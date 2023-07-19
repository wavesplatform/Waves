package com.wavesplatform.ride.runner.entrypoints

import com.google.protobuf.UnsafeByteOperations
import com.wavesplatform.account.Alias
import com.wavesplatform.api.http.utils.UtilsEvaluator
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.history.DefaultBlockchainSettings
import com.wavesplatform.ride.runner.TestScript
import com.wavesplatform.ride.runner.blockchain.ImmutableBlockchain
import com.wavesplatform.ride.runner.input.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxNonNegativeAmount
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}
import play.api.libs.json.Json

import java.nio.charset.StandardCharsets

class PreparedStateTestSuite extends BaseTestSuite with HasTestAccounts {
  private val btc  = IssuedAsset(ByteStr.decodeBase58("8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS").get)
  private val txId = ByteStr.decodeBase58("8rc5Asw43qbq7LMZ6tu2aVbVkw72XmBt7tTnwMSNfaNq").get

  private lazy val defaultInput = RideRunnerInput(
    address = scriptedAccAddr,
    request = Json.obj("expr" -> "foo(1)"),
    state = RideRunnerBlockchainState(
      height = 3296627,
      accounts = Map(
        scriptedAccAddr -> RideRunnerAccount(
          scriptInfo = Some(RideRunnerScriptInfo(script = mkAccountScript(hasPayments = false))),
          assetBalances = Map(btc -> TxNonNegativeAmount(1)),
          regularBalance = Some(TxNonNegativeAmount(500_000))
        ),
        aliceAddr -> RideRunnerAccount(
          data = Some(Map("a" -> IntegerRideRunnerDataEntry(11))),
          aliases = List(Alias.create("carl").explicitGet()),
          assetBalances = Map(btc -> TxNonNegativeAmount(2)),
          regularBalance = Some(TxNonNegativeAmount(1_300_000))
        ),
        bobAddr -> RideRunnerAccount(
          data = Some(Map.empty),
          assetBalances = Map(btc -> TxNonNegativeAmount(3)),
          leasing = Some(RideRunnerLeaseBalance(in = TxNonNegativeAmount(10), out = TxNonNegativeAmount(100)))
        )
      ),
      assets = Map(
        btc -> RideRunnerAsset(
          description = StringOrBytesAsByteString(UnsafeByteOperations.unsafeWrap("Bitcoin".getBytes(StandardCharsets.UTF_8))),
          reissuable = true,
          quantity = 21000000
        )
      ),
      blocks = Map(
        3296627 -> RideRunnerBlock(
          timestamp = 1663299568885L,
          VRF = Some(ByteStr.decodeBase58("GHC3DQuW9ncm5sNy5u3TVEF4CXu1fsLVHVHYxJzuZr7b").get)
        )
      ),
      transactions = Map(
        txId -> RideRunnerTransaction(
          amount = 93119130,
          recipient = bobAddr,
          timestamp = 1663299600039L,
          height = Some(3281000)
        )
      )
    )
  )

  "PreparedState" - {
    "expr" in {
      val input      = defaultInput
      val blockchain = new ImmutableBlockchain(DefaultBlockchainSettings, input.state)

      val apiResult = UtilsEvaluator.evaluate(
        blockchain = blockchain,
        dAppAddress = input.address,
        request = input.request,
        options = UtilsEvaluator.EvaluateOptions(
          evaluateScriptComplexityLimit = Int.MaxValue,
          maxTxErrorLogSize = 0,
          enableTraces = input.trace,
          intAsString = true
        )
      )

      withClue(Json.prettyPrint(apiResult)) {
        (apiResult \ "result" \ "value" \ "_2" \ "value").as[BigInt] shouldBe 9007199362331057L
      }
    }

    "invoke" in {
      val state = defaultInput.state.copy(
        accounts = defaultInput.state.accounts.updated(
          scriptedAccAddr,
          RideRunnerAccount(
            scriptInfo = Some(
              RideRunnerScriptInfo(
                script = mkAccountScript(hasPayments = true),
                publicKey = scriptedAcc.publicKey
              )
            ),
            assetBalances = Map(btc -> TxNonNegativeAmount(5)),
            regularBalance = Some(TxNonNegativeAmount(5 * 1_700_000 + 2))
          )
        )
      )
      val blockchain = new ImmutableBlockchain(DefaultBlockchainSettings, state)

      val apiResult = UtilsEvaluator.evaluate(
        blockchain = blockchain,
        dAppAddress = defaultInput.address,
        request = Json.obj( // See UtilsInvocationRequest
          "call" -> Json.obj(
            "function" -> "foo",
            "args"     -> List(Json.obj("type" -> "integer", "value" -> 1))
          ),
          "senderPublicKey" -> alice.publicKey.toString,
          "payment"         -> List(Json.obj("amount" -> 1, "assetId" -> btc.toString)),
          "fee"             -> 1_300_000
        ),
        options = UtilsEvaluator.EvaluateOptions(
          evaluateScriptComplexityLimit = Int.MaxValue,
          maxTxErrorLogSize = 0,
          enableTraces = defaultInput.trace,
          intAsString = true
        )
      )

      withClue(Json.prettyPrint(apiResult)) {
        (apiResult \ "result" \ "value" \ "_2" \ "value").as[BigInt] shouldBe 9007199362331058L
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

  ([ScriptTransfer(bob, 1, asset)], x + x1 + x2 + x3 + x4 + x5 +  x6 + x7 + x8 + x9 + x10 + y1 + y2 ${if (hasPayments) "+ y3" else ""} + 9007199254740991)
}

@Callable(inv)
func bar() = {
  let x1 = if (valueOrElse(getBoolean("b"), false)) then 1 else 0
  ([], x1)
}""")

}
