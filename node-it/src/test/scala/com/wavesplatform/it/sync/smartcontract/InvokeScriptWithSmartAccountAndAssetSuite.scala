package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.api.http.ApiError.{ScriptExecutionError, TransactionNotAllowedByAssetScript}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{issueFee, minFee, smartFee, smartMinFee}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class InvokeScriptWithSmartAccountAndAssetSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val estimator = ScriptEstimatorV2

  private def dApp        = firstKeyPair
  private def caller      = secondKeyPair
  private def smartCaller = thirdKeyPair

  var asset1: String = ""
  var asset2: String = ""
  var asset3: String = ""

  private lazy val dAppAddress: String        = dApp.toAddress.toString
  private lazy val callerAddress: String      = caller.toAddress.toString
  private lazy val smartCallerAddress: String = smartCaller.toAddress.toString

  test("invoke by smart account requires just 1 extra fee") {
    assertBadRequestAndMessage(
      sender.invokeScript(
        smartCaller,
        dAppAddress,
        Some("justWriteData"),
        fee = 0.00899999.waves
      ),
      s"does not exceed minimal value of 900000 WAVES"
    )
  }

  test("max fee is 0.053 Waves (0.005 + extraFee(1 smart caller + 1 payment + 10 transfers))") {
    val paymentAmount = 20

    assertApiError(
      sender
        .invokeScript(
          smartCaller,
          dAppAddress,
          Some("spendMaxFee"),
          payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(asset2).get))),
          fee = 0.05299999.waves
        ),
      AssertiveApiError(
        ScriptExecutionError.Id,
        "Error while executing dApp: Fee in WAVES for InvokeScriptTransaction (5299999 in WAVES) with 12 total scripts invoked does not exceed minimal value of 5300000 WAVES."
      )
    )

    val invokeScriptTxId = sender
      .invokeScript(
        smartCaller,
        dAppAddress,
        Some("spendMaxFee"),
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(asset2).get))),
        fee = 5300000
      )
      ._1
      .id
    nodes.waitForHeightAriseAndTxPresent(invokeScriptTxId)

    sender.stateChanges(invokeScriptTxId).stateChanges.get.error shouldBe empty
  }

  test("can't invoke with insufficient payment for @Verifier") {
    val amountLessThanVerifierLimit = 12

    assertBadRequestAndMessage(
      sender
        .invokeScript(
          smartCaller,
          dAppAddress,
          Some("get10ofAsset1"),
          payment = Seq(Payment(amountLessThanVerifierLimit, IssuedAsset(ByteStr.decodeBase58(asset2).get))),
          fee = smartMinFee + smartFee + smartFee // scripted asset + dApp
        )
        ._1
        .id,
      "Transaction is not allowed by account-script"
    )
  }

  test("can't invoke with small fee for both smart assets") {
    val amountGreaterThanAccountScriptLimit = 20

    assertBadRequestAndMessage(
      sender
        .invokeScript(
          caller,
          dAppAddress,
          Some("payAsset2GetAsset1"),
          payment = Seq(Payment(amountGreaterThanAccountScriptLimit, IssuedAsset(ByteStr.decodeBase58(asset2).get))),
          fee = smartMinFee
        )
        ._1
        .id,
      "does not exceed minimal value of 900000 WAVES"
    )
  }

  test("can't invoke with small fee for one of smart assets") {
    val amountGreaterThanAccountScriptLimit = 20

    assertApiError(
      sender
        .invokeScript(
          caller,
          dAppAddress,
          Some("payAsset2GetAsset1"),
          payment = Seq(Payment(amountGreaterThanAccountScriptLimit, IssuedAsset(ByteStr.decodeBase58(asset2).get))),
          fee = smartMinFee + smartFee
        ),
      AssertiveApiError(
        ScriptExecutionError.Id,
        "Error while executing dApp: Fee in WAVES for InvokeScriptTransaction (900000 in WAVES) with 2 total scripts invoked does not exceed minimal value of 1300000 WAVES."
      )
    )
  }

  test("can invoke a function with enough payment and fee") {
    val amountGreaterThanAccountScriptLimit = 20

    val invokeScriptId = sender
      .invokeScript(
        caller,
        dAppAddress,
        Some("payAsset2GetAsset1"),
        payment = Seq(Payment(amountGreaterThanAccountScriptLimit, IssuedAsset(ByteStr.decodeBase58(asset2).get))),
        fee = smartMinFee + smartFee + smartFee
      )
      ._1
      .id

    nodes.waitForHeightAriseAndTxPresent(invokeScriptId)
  }

  test("can't invoke with payment if asset script disallows InvokeScript") {
    val amountGreaterThanDAppScriptLimit = 16

    assertApiError(
      sender
        .invokeScript(
          caller,
          dAppAddress,
          Some("payAsset1GetAsset2"),
          payment = Seq(Payment(amountGreaterThanDAppScriptLimit, IssuedAsset(ByteStr.decodeBase58(asset1).get))),
          fee = smartMinFee + smartFee + smartFee
        ),
      AssertiveApiError(TransactionNotAllowedByAssetScript.Id, "Transaction is not allowed by token-script")
    )
  }

  test("can't invoke a function with payment less than dApp script's limit") {
    val amountLessThanDAppScriptLimit = 15

    assertApiError(
      sender
        .invokeScript(
          caller,
          dAppAddress,
          Some("payAsset2GetAsset1"),
          payment = Seq(Payment(amountLessThanDAppScriptLimit, IssuedAsset(ByteStr.decodeBase58(asset2).get))),
          fee = smartMinFee + smartFee + smartFee
        ),
      AssertiveApiError(ScriptExecutionError.Id, s"Error while executing dApp: need payment in 15+ tokens of asset2 $asset2")
    )
  }

  test("can't invoke a function with payment less than asset script's limit") {
    val amountGreaterThanDAppScriptLimit = 16

    assertApiError(
      sender.invokeScript(
        caller,
        dAppAddress,
        Some("payAsset2GetAsset3"),
        payment = Seq(Payment(amountGreaterThanDAppScriptLimit, IssuedAsset(ByteStr.decodeBase58(asset2).get))),
        fee = smartMinFee + smartFee + smartFee
      ),
      AssertiveApiError(TransactionNotAllowedByAssetScript.Id, "Transaction is not allowed by token-script")
    )
  }

  test("can't invoke a function that transfers less than asset script's limit") {
    assertApiError(
      sender.invokeScript(caller, dAppAddress, Some("get10ofAsset1"), fee = smartMinFee + smartFee),
      AssertiveApiError(TransactionNotAllowedByAssetScript.Id, "Transaction is not allowed by token-script")
    )
  }

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    withClue("send waves to dApp and caller accounts") {
      val dAppTransferId        = sender.transfer(sender.keyPair, dAppAddress, 5.waves, minFee).id
      val callerTransferId      = sender.transfer(sender.keyPair, callerAddress, 5.waves, minFee).id
      val smartCallerTransferId = sender.transfer(sender.keyPair, smartCallerAddress, 5.waves, minFee).id

      nodes.waitForHeightAriseAndTxPresent(smartCallerTransferId)
      nodes.waitForTransaction(callerTransferId)
      nodes.waitForTransaction(dAppTransferId)
    }

    withClue("issue and transfer smart assets between dApp and caller") {
      asset1 = sender
        .issue(
          dApp,
          "Asset1",
          "test asset",
          1500,
          0,
          reissuable = true,
          issueFee,
          script = Some(
            ScriptCompiler
              .compile(
                s"""
                   |match tx {
                   |  case tx:TransferTransaction => tx.amount > 10
                   |  case _ => false
                   |}""".stripMargin,
                estimator
              )
              .explicitGet()
              ._1
              .bytes()
              .base64
          )
        )
        .id

      asset2 = sender
        .issue(
          dApp,
          "Asset2",
          "test asset",
          1500,
          0,
          reissuable = true,
          issueFee,
          script = Some(
            ScriptCompiler
              .compile(
                s"""
                   |{-# STDLIB_VERSION 3 #-}
                   |match tx {
                   |  case tx:InvokeScriptTransaction => extract(tx.payment).amount > 10
                   |  case _:TransferTransaction => true
                   |  case _ => false
                   |}""".stripMargin,
                estimator
              )
              .explicitGet()
              ._1
              .bytes()
              .base64
          )
        )
        .id

      asset3 = sender
        .issue(
          dApp,
          "Asset3",
          "test asset",
          1500,
          0,
          reissuable = true,
          issueFee,
          script = Some(
            ScriptCompiler
              .compile(
                s"""
                   |{-# STDLIB_VERSION 3 #-}
                   |match tx {
                   |  case tx:TransferTransaction => tx.amount > 20
                   |  case _ => false
                   |}""".stripMargin,
                estimator
              )
              .explicitGet()
              ._1
              .bytes()
              .base64
          )
        )
        .id

      nodes.waitForHeightAriseAndTxPresent(asset3)
      nodes.waitForHeightAriseAndTxPresent(asset2)
      sender.waitForTransaction(asset1)

      val asset1ToCallerId = sender.transfer(dApp, callerAddress, 500, smartMinFee, Some(asset1)).id
      val asset2ToCallerId = sender.transfer(dApp, callerAddress, 500, smartMinFee, Some(asset2)).id
      val asset3ToCallerId = sender.transfer(dApp, callerAddress, 500, smartMinFee, Some(asset3)).id
      val asset1ToSmartId  = sender.transfer(dApp, smartCallerAddress, 500, smartMinFee, Some(asset1)).id
      val asset2ToSmartId  = sender.transfer(dApp, smartCallerAddress, 500, smartMinFee, Some(asset2)).id
      val asset3ToSmartId  = sender.transfer(dApp, smartCallerAddress, 500, smartMinFee, Some(asset3)).id
      nodes.waitForHeightAriseAndTxPresent(asset2ToSmartId)
      nodes.waitForHeightAriseAndTxPresent(asset3ToSmartId)
      sender.waitForTransaction(asset1ToCallerId)
      sender.waitForTransaction(asset2ToCallerId)
      sender.waitForTransaction(asset3ToCallerId)
      sender.waitForTransaction(asset1ToSmartId)
    }

    withClue("set scripts to dApp and smartCaller account") {
      val dAppScript = ScriptCompiler
        .compile(
          s"""
             |{-# STDLIB_VERSION 3 #-}
             |{-# CONTENT_TYPE DAPP #-}
             |
             |let asset1 = base58'$asset1'
             |let asset2 = base58'$asset2'
             |let asset3 = base58'$asset3'
             |
             |@Callable(i)
             |func payAsset1GetAsset2() = {
             |  let pay = extract(i.payment)
             |  if (pay.assetId == asset1 && pay.amount > 15) then
             |    TransferSet([ScriptTransfer(i.caller, 15, asset2)])
             |  else throw("need payment in 15+ tokens of asset1 " + toBase58String(asset1))
             |}
             |
             |@Callable(i)
             |func payAsset2GetAsset1() = {
             |  let pay = extract(i.payment)
             |  if (pay.assetId == asset2 && pay.amount > 15) then
             |    TransferSet([ScriptTransfer(i.caller, 15, asset1)])
             |  else {
             |    if (${"sigVerify(base58'', base58'', base58'') ||" * 16} true)
             |    then
             |       throw("need payment in 15+ tokens of asset2 " + toBase58String(asset2))
             |    else
             |       throw("unexpected")
             |  }
             |}
             |@Callable(i)
             |func payAsset2GetAsset3() = {
             |  let pay = extract(i.payment)
             |  if (pay.assetId == asset2 && pay.amount > 15) then
             |    TransferSet([ScriptTransfer(i.caller, 15, asset3)])
             |  else throw("need payment in 15+ tokens of asset2 " + toBase58String(asset2))
             |}
             |
             |@Callable(i)
             |func get10ofAsset1() = {
             |  TransferSet([ScriptTransfer(i.caller, 10, asset1)])
             |}
             |
             |@Callable(i)
             |func spendMaxFee() = {
             |  if (extract(i.payment).assetId == asset2) then
             |    TransferSet([
             |      ScriptTransfer(i.caller, 11, asset1),
             |      ScriptTransfer(i.caller, 11, asset1),
             |      ScriptTransfer(i.caller, 11, asset1),
             |      ScriptTransfer(i.caller, 11, asset1),
             |      ScriptTransfer(i.caller, 11, asset1),
             |      ScriptTransfer(i.caller, 11, asset1),
             |      ScriptTransfer(i.caller, 11, asset1),
             |      ScriptTransfer(i.caller, 11, asset1),
             |      ScriptTransfer(i.caller, 11, asset1),
             |      ScriptTransfer(i.caller, 11, asset1)
             |    ])
             |  else throw("need payment in asset2 " + toBase58String(asset2))
             |}
             |
             |@Callable(i)
             |func justWriteData() = {
             |  WriteSet([DataEntry("a", "a")])
             |}
        """.stripMargin,
          estimator
        )
        .explicitGet()
        ._1
      val dAppSetScriptTxId = sender.setScript(dApp, Some(dAppScript.bytes().base64)).id

      val smartCallerScript = ScriptCompiler
        .compile(
          """
            |{-# STDLIB_VERSION 3 #-}
            |{-# CONTENT_TYPE DAPP #-}
            |
            |@Verifier(tx)
            |func verify() = {
            |  match (tx) {
            |    case tx:InvokeScriptTransaction =>
            |      if (isDefined(tx.payment)) then
            |        extract(tx.payment).amount > 12
            |      else true
            |    case _ => false
            |  }
            |}
        """.stripMargin,
          estimator
        )
        .explicitGet()
        ._1
      val smartCallerSetScriptTxId = sender.setScript(smartCaller, Some(smartCallerScript.bytes().base64)).id

      nodes.waitForHeightAriseAndTxPresent(smartCallerSetScriptTxId)
      sender.waitForTransaction(dAppSetScriptTxId)

      val dAppScriptInfo = sender.addressScriptInfo(dAppAddress)
      dAppScriptInfo.script.isEmpty shouldBe false
      dAppScriptInfo.scriptText.isEmpty shouldBe false
      dAppScriptInfo.script.get.startsWith("base64:") shouldBe true
      val smartCallerScriptInfo = sender.addressScriptInfo(smartCallerAddress)
      smartCallerScriptInfo.script.isEmpty shouldBe false
      smartCallerScriptInfo.scriptText.isEmpty shouldBe false
      smartCallerScriptInfo.script.get.startsWith("base64:") shouldBe true
    }
  }
}
