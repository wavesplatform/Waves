package com.wavesplatform.it.sync.grpc

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import io.grpc.Status.Code

import scala.concurrent.duration._

class SetAssetScriptGrpcSuite extends GrpcBaseTransactionSuite {
  val estimator = ScriptEstimatorV2

  var assetWOScript = ""
  var assetWScript  = ""
  private val unchangeableScript = ScriptCompiler(
    s"""
       |match tx {
       |  case _: SetAssetScriptTransaction => false
       |  case _ => true
       |}
       """.stripMargin,
    isAssetScript = true,
    estimator
  ).explicitGet()._1

  test("issuer cannot change script on asset w/o initial script") {
    val firstBalance    = miner.wavesBalance(firstAddress).available
    val firstEffBalance = miner.wavesBalance(firstAddress).effective

    assertGrpcError(
      miner.setAssetScript(firstAcc, assetWOScript, Right(Some(script)), setAssetScriptFee),
      "Cannot set script on an asset issued without a script",
      Code.INVALID_ARGUMENT
    )
    assertGrpcError(
      miner.setAssetScript(firstAcc, assetWOScript, Right(None), setAssetScriptFee),
      "Cannot set empty script",
      Code.INVALID_ARGUMENT
    )

    miner.wavesBalance(firstAddress).available shouldBe firstBalance
    miner.wavesBalance(firstAddress).effective shouldBe firstEffBalance
  }

  test("non-issuer cannot change script") {
    val assetWAnotherOwner = PBTransactions
      .vanilla(
        miner.broadcastIssue(
          source = firstAcc,
          name = "NonOwnCoin",
          quantity = someAssetAmount,
          decimals = 0,
          reissuable = false,
          fee = issueFee,
          script = Right(
            Some(
              ScriptCompiler(
                s"""
               |match tx {
               |  case s: SetAssetScriptTransaction => s.sender == addressFromPublicKey(base58'${secondAcc.publicKey}')
               |  case _ => false
               |}
               """.stripMargin,
                isAssetScript = true,
                estimator
              ).explicitGet()._1
            )
          ),
          waitForTx = true
        )
      )
      .explicitGet()
      .id()
      .toString

    assertGrpcError(
      miner.setAssetScript(secondAcc, assetWAnotherOwner, Right(Some(script)), setAssetScriptFee),
      "Asset was issued by other address",
      Code.INVALID_ARGUMENT
    )
  }

  test("sender's waves balance is decreased by fee") {
    val script2 = ScriptCompiler(
      s"""
         |match tx {
         |  case _: SetAssetScriptTransaction => true
         |  case _ => false
         |}
         """.stripMargin,
      isAssetScript = true,
      estimator
    ).explicitGet()._1

    val firstBalance    = miner.wavesBalance(firstAddress).available
    val firstEffBalance = miner.wavesBalance(firstAddress).effective

    miner.setAssetScript(firstAcc, assetWScript, Right(Some(script2)), setAssetScriptFee, waitForTx = true)
    miner.assetInfo(assetWScript).script.flatMap(sd => PBTransactions.toVanillaScript(sd.scriptBytes)) should contain (script2)

    miner.wavesBalance(firstAddress).available shouldBe firstBalance - setAssetScriptFee
    miner.wavesBalance(firstAddress).effective shouldBe firstEffBalance - setAssetScriptFee
  }

  test("not able set script without having enough waves") {
    val firstBalance    = miner.wavesBalance(firstAddress).available
    val firstEffBalance = miner.wavesBalance(firstAddress).effective
    assertGrpcError(
      miner.setAssetScript(firstAcc, assetWScript, Right(Some(script)), fee = firstBalance + 1),
      "Accounts balance errors",
      Code.INVALID_ARGUMENT
    )

    miner.wavesBalance(firstAddress).available shouldBe firstBalance
    miner.wavesBalance(firstAddress).effective shouldBe firstEffBalance
  }

  test("not able to broadcast invalid set script transaction") {
    val firstBalance    = miner.wavesBalance(firstAddress).available
    val firstEffBalance = miner.wavesBalance(firstAddress).effective

    assertGrpcError(
      miner.setAssetScript(firstAcc, assetWScript, Right(Some(script)), setAssetScriptFee, timestamp = System.currentTimeMillis() + 1.day.toMillis),
      "Transaction timestamp .* is more than .*ms in the future",
      Code.INVALID_ARGUMENT
    )
    assertGrpcError(
      miner.setAssetScript(thirdAcc, assetWScript, Right(Some(script)), setAssetScriptFee - 1),
      "Fee .* does not exceed minimal value",
      Code.INVALID_ARGUMENT
    )
    assertGrpcError(
      miner.setAssetScript(firstAcc, "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz", Right(Some(script)), setAssetScriptFee),
      "Referenced assetId not found",
      Code.INVALID_ARGUMENT
    )

    miner.waitForHeight(miner.height + 1)
    miner.wavesBalance(firstAddress).available shouldBe firstBalance
    miner.wavesBalance(firstAddress).effective shouldBe firstEffBalance
  }

  test("try to make SetAssetScript tx on script that deprecates SetAssetScript") {
    val assetUnchangeableScript =
      PBTransactions
        .vanilla(
          miner.broadcastIssue(
            source = firstAcc,
            name = "SetAssetWDep",
            someAssetAmount,
            2,
            reissuable = false,
            issueFee,
            script = Right(Some(unchangeableScript)),
            waitForTx = true
          )
        )
        .explicitGet()
        .id()
        .toString

    assertGrpcError(
      miner.setAssetScript(firstAcc, assetUnchangeableScript, Right(Some(script)), setAssetScriptFee),
      "Transaction is not allowed by token-script",
      Code.INVALID_ARGUMENT
    )
  }

  test("try to make SetAssetScript for asset v1") {
    val assetV1 = PBTransactions
      .vanilla(
        miner.broadcastIssue(thirdAcc, "assetV1", someAssetAmount, 8, reissuable = true, issueFee, waitForTx = true)
      )
      .explicitGet()
      .id()
      .toString

    val balance    = miner.wavesBalance(thirdAddress).available
    val effBalance = miner.wavesBalance(thirdAddress).effective

    assertGrpcError(
      miner.setAssetScript(thirdAcc, assetV1, Right(Some(script)), setAssetScriptFee),
      "Reason: Cannot set script on an asset issued without a script",
      Code.INVALID_ARGUMENT
    )

    miner.wavesBalance(thirdAddress).available shouldBe balance
    miner.wavesBalance(thirdAddress).effective shouldBe effBalance

  }


  protected override def beforeAll(): Unit = {
    super.beforeAll()
    assetWOScript = PBTransactions
      .vanilla(
        miner.broadcastIssue(
          source = firstAcc,
          name = "AssetWOScript",
          quantity = someAssetAmount,
          decimals = 0,
          reissuable = false,
          fee = issueFee,
          waitForTx = true
        )
      )
      .explicitGet()
      .id()
      .toString

    assetWScript = PBTransactions
      .vanilla(
        miner.broadcastIssue(
          source = firstAcc,
          name = "AssetWOScript",
          quantity = someAssetAmount,
          decimals = 0,
          reissuable = false,
          fee = issueFee,
          script = Right(Some(script)),
          waitForTx = true
        )
      )
      .explicitGet()
      .id()
      .toString
  }
}
