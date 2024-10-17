package com.wavesplatform.it.sync.grpc

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncGrpcApi.*
import com.wavesplatform.it.sync.*
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.protobuf.transaction.PBTransactions
import io.grpc.Status.Code

import scala.concurrent.duration.*

class SetAssetScriptGrpcSuite extends GrpcBaseTransactionSuite {
  var assetWOScript = ""
  var assetWScript  = ""
  private val unchangeableScript = TestCompiler.DefaultVersion.compileAsset(
    s"""
       |match tx {
       |  case _: SetAssetScriptTransaction => false
       |  case _ => true
       |}
       """.stripMargin
  )

  test("issuer cannot change script on asset w/o initial script") {
    val firstBalance    = sender.wavesBalance(firstAddress).available
    val firstEffBalance = sender.wavesBalance(firstAddress).effective

    assertGrpcError(
      sender.setAssetScript(firstAcc, assetWOScript, Right(Some(script)), setAssetScriptFee),
      "Cannot set script on an asset issued without a script",
      Code.INVALID_ARGUMENT
    )
    assertGrpcError(
      sender.setAssetScript(firstAcc, assetWOScript, Right(None), setAssetScriptFee),
      "Cannot set empty script",
      Code.INVALID_ARGUMENT
    )

    sender.wavesBalance(firstAddress).available shouldBe firstBalance
    sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance
  }

  test("non-issuer cannot change script") {
    val assetWAnotherOwner = PBTransactions
      .vanilla(
        sender.broadcastIssue(
          source = firstAcc,
          name = "NonOwnCoin",
          quantity = someAssetAmount,
          decimals = 0,
          reissuable = false,
          fee = issueFee,
          script = Right(
            Some(
              TestCompiler.DefaultVersion.compileAsset(
                s"""
                   |match tx {
                   |  case s: SetAssetScriptTransaction => s.sender == addressFromPublicKey(base58'${secondAcc.publicKey}')
                   |  case _ => false
                   |}
               """.stripMargin
              )
            )
          ),
          waitForTx = true
        ),
        unsafe = false
      )
      .explicitGet()
      .id()
      .toString

    assertGrpcError(
      sender.setAssetScript(secondAcc, assetWAnotherOwner, Right(Some(script)), setAssetScriptFee),
      "Asset was issued by other address",
      Code.INVALID_ARGUMENT
    )
  }

  test("sender's waves balance is decreased by fee") {
    val script2 = TestCompiler.DefaultVersion.compileAsset(
      s"""
         |match tx {
         |  case _: SetAssetScriptTransaction => true
         |  case _ => false
         |}
         """.stripMargin
    )

    val firstBalance    = sender.wavesBalance(firstAddress).available
    val firstEffBalance = sender.wavesBalance(firstAddress).effective

    sender.setAssetScript(firstAcc, assetWScript, Right(Some(script2)), setAssetScriptFee, waitForTx = true)
    sender.assetInfo(assetWScript).script.flatMap(sd => PBTransactions.toVanillaScript(sd.scriptBytes)) should contain(script2)

    sender.wavesBalance(firstAddress).available shouldBe firstBalance - setAssetScriptFee
    sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance - setAssetScriptFee
  }

  test("not able set script without having enough waves") {
    val firstBalance    = sender.wavesBalance(firstAddress).available
    val firstEffBalance = sender.wavesBalance(firstAddress).effective
    assertGrpcError(
      sender.setAssetScript(firstAcc, assetWScript, Right(Some(script)), fee = firstBalance + 1),
      "Accounts balance errors",
      Code.INVALID_ARGUMENT
    )

    sender.wavesBalance(firstAddress).available shouldBe firstBalance
    sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance
  }

  test("not able to broadcast invalid set script transaction") {
    val firstBalance    = sender.wavesBalance(firstAddress).available
    val firstEffBalance = sender.wavesBalance(firstAddress).effective

    assertGrpcError(
      sender.setAssetScript(firstAcc, assetWScript, Right(Some(script)), setAssetScriptFee, timestamp = System.currentTimeMillis() + 1.day.toMillis),
      "Transaction timestamp .* is more than .*ms in the future",
      Code.INVALID_ARGUMENT
    )
    assertGrpcError(
      sender.setAssetScript(thirdAcc, assetWScript, Right(Some(script)), setAssetScriptFee - 1),
      "Fee .* does not exceed minimal value",
      Code.INVALID_ARGUMENT
    )
    assertGrpcError(
      sender.setAssetScript(firstAcc, "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz", Right(Some(script)), setAssetScriptFee),
      "Referenced assetId not found",
      Code.INVALID_ARGUMENT
    )

    sender.waitForHeight(sender.height + 1)
    sender.wavesBalance(firstAddress).available shouldBe firstBalance
    sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance
  }

  test("try to make SetAssetScript tx on script that deprecates SetAssetScript") {
    val assetUnchangeableScript =
      PBTransactions
        .vanilla(
          sender.broadcastIssue(
            source = firstAcc,
            name = "SetAssetWDep",
            someAssetAmount,
            2,
            reissuable = false,
            issueFee,
            script = Right(Some(unchangeableScript)),
            waitForTx = true
          ),
          unsafe = false
        )
        .explicitGet()
        .id()
        .toString

    assertGrpcError(
      sender.setAssetScript(firstAcc, assetUnchangeableScript, Right(Some(script)), setAssetScriptFee),
      "Transaction is not allowed by token-script",
      Code.INVALID_ARGUMENT
    )
  }

  test("try to make SetAssetScript for asset v1") {
    val assetV1 = PBTransactions
      .vanilla(
        sender.broadcastIssue(thirdAcc, "assetV1", someAssetAmount, 8, reissuable = true, issueFee, waitForTx = true),
        unsafe = false
      )
      .explicitGet()
      .id()
      .toString

    val balance    = sender.wavesBalance(thirdAddress).available
    val effBalance = sender.wavesBalance(thirdAddress).effective

    assertGrpcError(
      sender.setAssetScript(thirdAcc, assetV1, Right(Some(script)), setAssetScriptFee),
      "Reason: Cannot set script on an asset issued without a script",
      Code.INVALID_ARGUMENT
    )

    sender.wavesBalance(thirdAddress).available shouldBe balance
    sender.wavesBalance(thirdAddress).effective shouldBe effBalance

  }

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    assetWOScript = PBTransactions
      .vanilla(
        sender.broadcastIssue(
          source = firstAcc,
          name = "AssetWOScript",
          quantity = someAssetAmount,
          decimals = 0,
          reissuable = false,
          fee = issueFee,
          waitForTx = true
        ),
        unsafe = false
      )
      .explicitGet()
      .id()
      .toString

    assetWScript = PBTransactions
      .vanilla(
        sender.broadcastIssue(
          source = firstAcc,
          name = "AssetWOScript",
          quantity = someAssetAmount,
          decimals = 0,
          reissuable = false,
          fee = issueFee,
          script = Right(Some(script)),
          waitForTx = true
        ),
        unsafe = false
      )
      .explicitGet()
      .id()
      .toString
  }
}
