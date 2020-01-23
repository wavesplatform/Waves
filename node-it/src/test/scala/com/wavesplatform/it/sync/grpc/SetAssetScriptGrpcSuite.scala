package com.wavesplatform.it.sync.grpc

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.transaction.PBTransactions
import io.grpc.Status.Code

import scala.concurrent.duration._

class SetAssetScriptGrpcSuite extends GrpcBaseTransactionSuite {
  val estimator = ScriptEstimatorV2

  var assetWOScript    = ""
  var assetWScript     = ""
  private val unchangeableScript = ScriptCompiler(
    s"""
       |match tx {
       |  case s : SetAssetScriptTransaction => false
       |  case _ => true
       |}
       """.stripMargin,
    isAssetScript = true,
    estimator
  ).explicitGet()._1

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    assetWOScript = PBTransactions.vanilla(
      sender.broadcastIssue(
        source = firstAcc,
        name = "AssetWOScript",
        quantity = someAssetAmount,
        decimals = 0,
        reissuable = false,
        fee = issueFee,
        waitForTx = true)).explicitGet().id().base58

    assetWScript = PBTransactions.vanilla(
      sender.broadcastIssue(
        source = firstAcc,
        name = "AssetWOScript",
        quantity = someAssetAmount,
        decimals = 0,
        reissuable = false,
        fee = issueFee,
        script = Some(scriptBase64),
        waitForTx = true)).explicitGet().id().base58
  }

  test("issuer cannot change script on asset w/o initial script") {
    val firstBalance     = sender.wavesBalance(firstAddress).available
    val firstEffBalance  = sender.wavesBalance(firstAddress).effective

    assertGrpcError(
      sender.setAssetScript(firstAcc, assetWOScript, Some(scriptBase64), setAssetScriptFee),
      "Cannot set script on an asset issued without a script",
      Code.INVALID_ARGUMENT
    )
    assertGrpcError(
      sender.setAssetScript(firstAcc, assetWOScript, None, setAssetScriptFee),
      "Cannot set empty script",
      Code.INVALID_ARGUMENT
    )

    sender.wavesBalance(firstAddress).available shouldBe firstBalance
    sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance
  }

  test("non-issuer cannot change script") {
    val assetWAnotherOwner = PBTransactions.vanilla(
      sender.broadcastIssue(
        source = firstAcc,
        name = "NonOwnCoin",
        quantity = someAssetAmount,
        decimals = 0,
        reissuable = false,
        fee = issueFee,
        script = Some(
          ScriptCompiler(
            s"""
               |match tx {
               |  case s : SetAssetScriptTransaction => s.sender == addressFromPublicKey(base58'${ByteStr(secondAcc.publicKey).base58}')
               |  case _ => false
               |}
               """.stripMargin,
            isAssetScript = true,
            estimator
          ).explicitGet()._1.bytes.value.base64),
        waitForTx = true)).explicitGet().id().base58

    assertGrpcError(
      sender.setAssetScript(secondAcc, assetWAnotherOwner, Some(scriptBase64), setAssetScriptFee),
    "Asset was issued by other address",
      Code.INVALID_ARGUMENT
    )
  }

  test("sender's waves balance is decreased by fee") {
    val script2 = ScriptCompiler(
      s"""
         |match tx {
         |  case s : SetAssetScriptTransaction => true
         |  case _ => false
         |}
         """.stripMargin,
      isAssetScript = true,
      estimator
    ).explicitGet()._1.bytes.value.base64

    val firstBalance     = sender.wavesBalance(firstAddress).available
    val firstEffBalance  = sender.wavesBalance(firstAddress).effective

    sender.setAssetScript(firstAcc, assetWScript, Some(script2), setAssetScriptFee, waitForTx = true)

    sender.wavesBalance(firstAddress).available shouldBe firstBalance - setAssetScriptFee
    sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance - setAssetScriptFee
  }

  test("not able set script without having enough waves") {
    val firstBalance     = sender.wavesBalance(firstAddress).available
    val firstEffBalance  = sender.wavesBalance(firstAddress).effective
    assertGrpcError(
      sender.setAssetScript(firstAcc, assetWScript, Some(scriptBase64), fee = firstBalance + 1),
      "negative waves balance",
      Code.INVALID_ARGUMENT)

    sender.wavesBalance(firstAddress).available shouldBe firstBalance
    sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance
  }

  test("not able to broadcast invalid set script transaction") {
    val firstBalance     = sender.wavesBalance(firstAddress).available
    val firstEffBalance  = sender.wavesBalance(firstAddress).effective

    assertGrpcError(
      sender.setAssetScript(firstAcc, assetWScript, Some(scriptBase64),setAssetScriptFee, timestamp = System.currentTimeMillis() + 1.day.toMillis),
    "Transaction timestamp .* is more than .*ms in the future",
      Code.INVALID_ARGUMENT
    )
    assertGrpcError(
      sender.setAssetScript(thirdAcc, assetWScript, Some(scriptBase64),setAssetScriptFee - 1),
      "Fee .* does not exceed minimal value",
      Code.INVALID_ARGUMENT
    )
    assertGrpcError(
      sender.setAssetScript(firstAcc, "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz", Some(scriptBase64),setAssetScriptFee),
      "Referenced assetId not found",
      Code.INVALID_ARGUMENT
    )

    sender.waitForHeight(sender.height + 1)
    sender.wavesBalance(firstAddress).available shouldBe firstBalance
    sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance
  }

  test("try to make SetAssetScript tx on script that deprecates SetAssetScript") {
    val assetUnchangeableScript =
      PBTransactions.vanilla(
        sender.broadcastIssue(
        source = firstAcc,
        name = "SetAssetWDep",
        someAssetAmount,
        2,
        reissuable = false,
        issueFee,
        script = Some(unchangeableScript.bytes.value.base64),
        waitForTx = true
      )).explicitGet().id().base58

    assertGrpcError(
      sender.setAssetScript(firstAcc, assetUnchangeableScript, Some(scriptBase64), setAssetScriptFee),
      "Transaction is not allowed by token-script",
      Code.INVALID_ARGUMENT)
  }

  test("try to make SetAssetScript for asset v1") {
    val assetV1 = PBTransactions.vanilla(
      sender.broadcastIssue(thirdAcc, "assetV1", someAssetAmount, 8, reissuable = true, issueFee, waitForTx = true)
    ).explicitGet().id().base58

    val balance     = sender.wavesBalance(thirdAddress).available
    val effBalance  = sender.wavesBalance(thirdAddress).effective

    assertGrpcError(
      sender.setAssetScript(thirdAcc, assetV1, Some(scriptBase64), setAssetScriptFee),
      "Reason: Cannot set script on an asset issued without a script",
      Code.INVALID_ARGUMENT
    )

    sender.wavesBalance(thirdAddress).available shouldBe balance
    sender.wavesBalance(thirdAddress).effective shouldBe effBalance

  }


}
