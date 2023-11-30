package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.CONST_STRING
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state._
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class InvokeMultiplePaymentsSuite extends BaseTransactionSuite with CancelAfterFailure {
  private def dApp   = firstKeyPair
  private def caller = secondKeyPair

  private lazy val dAppAddress: String   = dApp.toAddress.toString
  private lazy val callerAddress: String = caller.toAddress.toString

  private var asset1: IssuedAsset = _
  private var asset2: IssuedAsset = _

  test("can transfer to alias") {
    val dAppBalance   = sender.balance(dAppAddress).balance
    val callerBalance = sender.balance(callerAddress).balance

    sender
      .invokeScript(
        caller,
        dAppAddress,
        Some("f"),
        payment = Seq(Payment(1.waves, Waves)),
        args = List(CONST_STRING("recipientalias").explicitGet()),
        waitForTx = true
      )

    sender.balance(dAppAddress).balance shouldBe dAppBalance
    sender.balance(callerAddress).balance shouldBe callerBalance - smartMinFee
  }

  test("script should sheck if alias not exist") {
    val alias = "unknown"

    assertBadRequestAndMessage(
      sender
        .invokeScript(
          caller,
          dAppAddress,
          Some("f"),
          payment = Seq(Payment(1.waves, Waves)),
          args = List(CONST_STRING(alias).explicitGet())
        ),
      s"Alias 'alias:I:$alias"
    )

    assertBadRequestAndMessage(
      sender
        .invokeScript(
          caller,
          dAppAddress,
          Some("f"),
          payment = Seq(Payment(1.waves, Waves)),
          args = List(CONST_STRING(s"alias:I:$alias").explicitGet()),
          waitForTx = true
        ),
      "Alias should contain only following characters"
    )
  }

  test("can invoke with no payments") {
    sender.invokeScript(caller, dAppAddress, payment = Seq.empty, waitForTx = true)
    sender.getData(dAppAddress).size shouldBe 0
  }

  test("can invoke with single payment of Waves") {
    sender.invokeScript(caller, dAppAddress, payment = Seq(Payment(1.waves, Waves)), waitForTx = true)
    sender.getData(dAppAddress).size shouldBe 2
    sender.getDataByKey(dAppAddress, "amount_0").as[IntegerDataEntry].value shouldBe 1.waves
    sender.getDataByKey(dAppAddress, "asset_0").as[BinaryDataEntry].value shouldBe ByteStr.empty
  }

  test("can invoke with single payment of asset") {
    sender.invokeScript(caller, dAppAddress, payment = Seq(Payment(10, asset1)), waitForTx = true)
    sender.getData(dAppAddress).size shouldBe 2
    sender.getDataByKey(dAppAddress, "amount_0").as[IntegerDataEntry].value shouldBe 10
    sender.getDataByKey(dAppAddress, "asset_0").as[BinaryDataEntry].value shouldBe asset1.id
  }

  test("can invoke with two payments of Waves") {
    sender.invokeScript(caller, dAppAddress, payment = Seq(Payment(5, Waves), Payment(17, Waves)), waitForTx = true)
    sender.getData(dAppAddress).size shouldBe 4
    sender.getDataByKey(dAppAddress, "amount_0").as[IntegerDataEntry].value shouldBe 5
    sender.getDataByKey(dAppAddress, "asset_0").as[BinaryDataEntry].value shouldBe ByteStr.empty
    sender.getDataByKey(dAppAddress, "amount_1").as[IntegerDataEntry].value shouldBe 17
    sender.getDataByKey(dAppAddress, "asset_1").as[BinaryDataEntry].value shouldBe ByteStr.empty
  }

  test("can invoke with two payments of the same asset") {
    sender.invokeScript(caller, dAppAddress, payment = Seq(Payment(8, asset1), Payment(21, asset1)), waitForTx = true)
    sender.getData(dAppAddress).size shouldBe 4
    sender.getDataByKey(dAppAddress, "amount_0").as[IntegerDataEntry].value shouldBe 8
    sender.getDataByKey(dAppAddress, "asset_0").as[BinaryDataEntry].value shouldBe asset1.id
    sender.getDataByKey(dAppAddress, "amount_1").as[IntegerDataEntry].value shouldBe 21
    sender.getDataByKey(dAppAddress, "asset_1").as[BinaryDataEntry].value shouldBe asset1.id
  }

  test("can invoke with two payments of different assets") {
    sender.invokeScript(caller, dAppAddress, payment = Seq(Payment(3, asset1), Payment(6, asset2)), waitForTx = true)
    sender.getData(dAppAddress).size shouldBe 4
    sender.getDataByKey(dAppAddress, "amount_0").as[IntegerDataEntry].value shouldBe 3
    sender.getDataByKey(dAppAddress, "asset_0").as[BinaryDataEntry].value shouldBe asset1.id
    sender.getDataByKey(dAppAddress, "amount_1").as[IntegerDataEntry].value shouldBe 6
    sender.getDataByKey(dAppAddress, "asset_1").as[BinaryDataEntry].value shouldBe asset2.id
  }

  test("can't invoke with three payments") {
    assertApiError(
      sender.invokeScript(
        caller,
        dAppAddress,
        payment = Seq(Payment(3, Waves), Payment(6, Waves), Payment(7, Waves))
      )
    ) { error =>
      error.message should include("Script payment amount=3 should not exceed 2")
      error.id shouldBe StateCheckFailed.Id
      error.statusCode shouldBe 400
    }
  }

  test("can't attach more than balance") {
    val wavesBalance  = sender.accountBalances(callerAddress)._1
    val asset1Balance = sender.assetBalance(callerAddress, asset1.id.toString).balance

    assertApiError(
      sender.invokeScript(
        caller,
        dAppAddress,
        payment = Seq(Payment(wavesBalance - 1.waves, Waves), Payment(2.waves, Waves))
      )
    ) { error =>
      error.message should include("Transaction application leads to negative waves balance to (at least) temporary negative state")
      error.id shouldBe StateCheckFailed.Id
      error.statusCode shouldBe 400
    }

    assertApiError(
      sender.invokeScript(
        caller,
        dAppAddress,
        payment = Seq(Payment(asset1Balance - 1000, asset1), Payment(1001, asset1))
      )
    ) { error =>
      error.message should include("Transaction application leads to negative asset")
      error.id shouldBe StateCheckFailed.Id
      error.statusCode shouldBe 400
    }
  }

  test("can't attach leased Waves") {
    val wavesBalance = sender.accountBalances(callerAddress)._1
    sender.lease(caller, dAppAddress, wavesBalance - 1.waves, waitForTx = true)

    assertApiError(
      sender.invokeScript(caller, dAppAddress, payment = Seq(Payment(0.75.waves, Waves), Payment(0.75.waves, Waves)))
    ) { error =>
      error.message should include("Accounts balance errors")
      error.id shouldBe StateCheckFailed.Id
      error.statusCode shouldBe 400
    }
  }

  test("can't attach with zero Waves amount") {
    assertApiError(
      sender.invokeScript(caller, dAppAddress, payment = Seq(Payment(1, asset1), Payment(0, Waves))),
      NonPositiveAmount("0 of Waves")
    )
  }

  test("can't attach with zero asset amount") {
    assertApiError(
      sender.invokeScript(caller, dAppAddress, payment = Seq(Payment(0, asset1), Payment(1, Waves))),
      NonPositiveAmount(s"0 of $asset1")
    )
  }

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    val source =
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |func parse(asset: ByteVector|Unit) = if asset.isDefined() then asset.value() else base58''
         |
         |@Callable(inv)
         |func default() = {
         |  let pmt = inv.payments
         |  nil
         |  ++ (if pmt.size() > 0 then [
         |    IntegerEntry("amount_0", pmt[0].amount),
         |    BinaryEntry("asset_0", pmt[0].assetId.parse())
         |  ] else nil)
         |  ++ (if pmt.size() > 1 then [
         |    IntegerEntry("amount_1", pmt[1].amount),
         |    BinaryEntry("asset_1", pmt[1].assetId.parse())
         |  ] else nil)
         |}
         |
         |@Callable(inv)
         |func f(toAlias: String) = {
         | if (${"sigVerify(base58'', base58'', base58'') ||" * 8} true)
         |  then {
         |    let pmt = inv.payments[0]
         |    #avoidbugcomment
         |    [ScriptTransfer(Alias(toAlias), pmt.amount, pmt.assetId)]
         |  }
         |  else {
         |    throw("unexpected")
         |  }
         |}
      """.stripMargin
    val script = ScriptCompiler.compile(source, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    sender.setScript(dApp, Some(script), setScriptFee, waitForTx = true)

    asset1 = IssuedAsset(ByteStr.decodeBase58(sender.issue(caller, waitForTx = true).id).get)
    asset2 = IssuedAsset(ByteStr.decodeBase58(sender.issue(caller, waitForTx = true).id).get)
    sender.createAlias(caller, "recipientalias", smartMinFee, waitForTx = true)
  }
}
