package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.api.http.ApiError.ScriptExecutionError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{minFee, smartMinFee}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class InvokeScriptWithSponsorshipSuite extends BaseTransactionSuite {
  private val estimator = ScriptEstimatorV2

  private def dApp = firstKeyPair

  private def caller = secondKeyPair

  val quantity: Long = 10000
  val halfQuantity: Long = quantity / 2
  var dAppAsset: String = ""
  var callerAsset: String = ""
  var smartAsset: String = ""
  var dAppInitBalance: Long   = 0
  var callerInitBalance: Long = 0

  private lazy val dAppAddress: String   = dApp.toAddress.toString
  private lazy val callerAddress: String = caller.toAddress.toString

  test("_send waves to dApp and caller accounts") {
    miner.transfer(miner.keyPair, dAppAddress, 5.waves, minFee, waitForTx = true).id
    miner.transfer(miner.keyPair, callerAddress, 5.waves, minFee, waitForTx = true).id

  }

  test("_issue and transfer assets") {
    dAppAsset = miner.issue(dApp, "dApp", "d", quantity, 0, waitForTx = true).id
    callerAsset = miner.issue(caller, "caller", "c", quantity, 0, waitForTx = true).id
    val script = Some(ScriptCompiler.compile("true", estimator).explicitGet()._1.bytes().base64)
    smartAsset = miner.issue(dApp, "Smart", "s", quantity, 0, script = script, waitForTx = true).id

    miner.transfer(dApp, callerAddress, halfQuantity, minFee, Some(dAppAsset), waitForTx = true).id
    miner.transfer(caller, dAppAddress, halfQuantity, minFee, Some(callerAsset), waitForTx = true).id
    miner.transfer(dApp, callerAddress, halfQuantity, smartMinFee, Some(smartAsset), waitForTx = true).id
  }

  test("_enable sponsorship") {
    miner.sponsorAsset(dApp, dAppAsset, 1, waitForTx = true).id
    miner.sponsorAsset(caller, callerAsset, 1, waitForTx = true).id
  }

  test("_set scripts to dApp and caller account") {
    val dAppScript = ScriptCompiler
      .compile(
        s"""
          |{-# STDLIB_VERSION 3 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |let dAppAsset = base58'$dAppAsset'
          |let callerAsset = base58'$callerAsset'
          |let smartAsset = base58'$smartAsset'
          |
          |@Callable(i)
          |func payCallerGetDAppAsset() = {
          |  if (isDefined(i.payment) && extract(i.payment).assetId == callerAsset) then
          |    TransferSet([
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset)
          |    ])
          |  else throw("need payment in callerAsset " + toBase58String(callerAsset))
          |}
          |
          |@Callable(i)
          |func spendMaxFee() = {
          |  if (isDefined(i.payment) && extract(i.payment).assetId == smartAsset) then
          |    TransferSet([
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset)
          |    ])
          |  else throw("need payment in smartAsset " + toBase58String(smartAsset))
          |}
        """.stripMargin,
        estimator
      )
      .explicitGet()
      ._1
    miner.setScript(dApp, Some(dAppScript.bytes().base64), waitForTx = true).id

    val callerScript = ScriptCompiler
      .compile(
        s"""
          |{-# STDLIB_VERSION 3 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Verifier(tx)
          |func verify() = {
          |  let callerAsset = base58'$callerAsset'
          |  let smartAsset = base58'$smartAsset'
          |  match (tx) {
          |    case tx:InvokeScriptTransaction =>
          |      let pay = extract(tx.payment)
          |      pay.assetId == callerAsset || pay.assetId == smartAsset
          |    case _ => false
          |  }
          |}
        """.stripMargin,
        estimator
      )
      .explicitGet()
      ._1
    miner.setScript(caller, Some(callerScript.bytes().base64), waitForTx = true).id

    val dAppScriptInfo = miner.addressScriptInfo(dAppAddress)
    dAppScriptInfo.script.isEmpty shouldBe false
    dAppScriptInfo.scriptText.isEmpty shouldBe false
    dAppScriptInfo.script.get.startsWith("base64:") shouldBe true

    val smartCallerScriptInfo = miner.addressScriptInfo(callerAddress)
    smartCallerScriptInfo.script.isEmpty shouldBe false
    smartCallerScriptInfo.scriptText.isEmpty shouldBe false
    smartCallerScriptInfo.script.get.startsWith("base64:") shouldBe true
  }

  test("required fee in sponsored assets considers scripts count") {
    dAppInitBalance = miner.wavesBalance(dAppAddress)
    callerInitBalance = miner.wavesBalance(callerAddress)
    val paymentAmount  = 1
    val feeAmount      = 9
    val smartFeeAmount = 53

    assertBadRequestAndMessage(
      miner.invokeScript(
        caller,
        dAppAddress,
        Some("payCallerGetDAppAsset"),
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(callerAsset).get))),
        fee = feeAmount - 1,
        feeAssetId = Some(dAppAsset)
      ),
      s"does not exceed minimal value of 900000 WAVES or $feeAmount"
    )

    assertApiError(
      miner
        .invokeScript(
          caller,
          dAppAddress,
          Some("spendMaxFee"),
          payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(smartAsset).get))),
          fee = smartFeeAmount - 1,
          feeAssetId = Some(dAppAsset)
        ),
      AssertiveApiError(ScriptExecutionError.Id, "with 12 total scripts invoked does not exceed minimal value", matchMessage = true)
    )

    miner
      .invokeScript(
        caller,
        dAppAddress,
        Some("payCallerGetDAppAsset"),
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(callerAsset).get))),
        fee = feeAmount,
        feeAssetId = Some(dAppAsset),
        waitForTx = true
      )

    miner
      .invokeScript(
        caller,
        dAppAddress,
        Some("spendMaxFee"),
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(smartAsset).get))),
        fee = smartFeeAmount,
        feeAssetId = Some(dAppAsset),
        waitForTx = true
      )

    miner.assetBalance(dAppAddress, dAppAsset).balance shouldBe halfQuantity + (feeAmount - 10) + smartFeeAmount
    miner.assetBalance(dAppAddress, callerAsset).balance shouldBe halfQuantity + paymentAmount
    miner.wavesBalance(dAppAddress) shouldBe dAppInitBalance - 0.009.waves - 0.053.waves

    miner.assetBalance(callerAddress, dAppAsset).balance shouldBe halfQuantity + (-feeAmount + 10) - smartFeeAmount
    miner.assetBalance(callerAddress, callerAsset).balance shouldBe halfQuantity - paymentAmount
    miner.wavesBalance(callerAddress) shouldBe callerInitBalance
  }

  test("dApp caller is dApp address") {
    val paymentAmount = 1
    val feeAmount     = 9

    val dAppAssetBalance = miner.assetBalance(dAppAddress, dAppAsset).balance
    val dAppWavesBalance = miner.wavesBalance(dAppAddress)
    miner
      .invokeScript(
        dApp,
        dAppAddress,
        Some("payCallerGetDAppAsset"),
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(callerAsset).get))),
        fee = feeAmount,
        feeAssetId = Some(dAppAsset),
        waitForTx = true
      )
      ._1
      .id

    miner.assetBalance(dAppAddress, dAppAsset).balance shouldBe dAppAssetBalance
    miner.wavesBalance(dAppAddress) shouldBe dAppWavesBalance - 0.009.waves
  }

}
