package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class InvokeMultiplePaymentsSuite extends BaseTransactionSuite with CancelAfterFailure {
  import InvokeMultiplePaymentsSuite._

//  override protected def nodeConfigs: Seq[Config] =
//    NodeConfigs.Builder(Default, 1, Seq.empty)
//      .overrideBase(_.quorum(0))
//      .overrideBase(_.preactivatedFeatures((16, 2)))
//      .buildNonConflicting()

  private val dApp = pkByAddress(firstAddress).stringRepr
  private val caller = pkByAddress(secondAddress).stringRepr

  private var asset1: IssuedAsset = _
  private var asset2: IssuedAsset = _

  test("prerequisite: set contract and issue asset") {
    val source =
    """
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
      |    IntEntry("amount_0", pmt[0].amount),
      |    BinaryEntry("asset_0", pmt[0].assetId.parse())
      |  ] else nil)
      |  ++ (if pmt.size() > 1 then [
      |    IntEntry("amount_1", pmt[1].amount),
      |    BinaryEntry("asset_1", pmt[1].assetId.parse())
      |  ] else nil)
      |}
      """.stripMargin
    val script = ScriptCompiler.compile(source, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    sender.setScript(dApp, Some(script), setScriptFee, waitForTx = true)

    asset1 = IssuedAsset(ByteStr.decodeBase58(sender.issue(caller, waitForTx = true).id).get)
    asset2 = IssuedAsset(ByteStr.decodeBase58(sender.issue(caller, waitForTx = true).id).get)
  }

  test("can invoke with no payments") {
    sender.invokeScript(caller, dApp, payment = Seq.empty, waitForTx = true)
    sender.getData(dApp).size shouldBe 0
  }

  test("can invoke with single payment of Waves") {
    sender.invokeScript(caller, dApp, payment = Seq(Payment(1.waves, Waves)), waitForTx = true)
    sender.getData(dApp).size shouldBe 2
    sender.getDataByKey(dApp, "amount_0") shouldBe IntegerDataEntry("amount_0", 1.waves)
    sender.getDataByKey(dApp, "asset_0") shouldBe BinaryDataEntry("asset_0", ByteStr.empty)
  }

  test("can invoke with single payment of asset") {
    sender.invokeScript(caller, dApp, payment = Seq(Payment(10, asset1)), waitForTx = true)
    sender.getData(dApp).size shouldBe 2
    sender.getDataByKey(dApp, "amount_0") shouldBe IntegerDataEntry("amount_0", 10)
    sender.getDataByKey(dApp, "asset_0") shouldBe BinaryDataEntry("asset_0", asset1.id)

    sender.getDataByKey(dApp, "asset_0").as[BinaryDataEntry].value shouldBe asset1.id //TODO test
    sender.getDataByKey(dApp, "asset_0").value shouldBe asset1.id //TODO test
  }

  test("can invoke with two payments of Waves") {
    sender.invokeScript(caller, dApp, payment = Seq(Payment(5, Waves), Payment(17, Waves)), waitForTx = true)
    sender.getData(dApp).size shouldBe 4
    sender.getDataByKey(dApp, "amount_0") shouldBe IntegerDataEntry("amount_0", 5)
    sender.getDataByKey(dApp, "asset_0") shouldBe BinaryDataEntry("asset_0", ByteStr.empty)
    sender.getDataByKey(dApp, "amount_1") shouldBe IntegerDataEntry("amount_1", 17)
    sender.getDataByKey(dApp, "asset_1") shouldBe BinaryDataEntry("asset_1", ByteStr.empty)
  }

  test("can invoke with two payments of the same asset") {
    sender.invokeScript(caller, dApp, payment = Seq(Payment(8, asset1), Payment(21, asset1)), waitForTx = true)
    sender.getData(dApp).size shouldBe 4
    sender.getDataByKey(dApp, "amount_0") shouldBe IntegerDataEntry("amount_0", 8)
    sender.getDataByKey(dApp, "asset_0") shouldBe BinaryDataEntry("asset_0", asset1.id)
    sender.getDataByKey(dApp, "amount_1") shouldBe IntegerDataEntry("amount_1", 21)
    sender.getDataByKey(dApp, "asset_1") shouldBe BinaryDataEntry("asset_1", asset1.id)
  }

  test("can invoke with two payments of different assets") {
    sender.invokeScript(caller, dApp, payment = Seq(Payment(3, asset1), Payment(6, asset2)), waitForTx = true)
    sender.getData(dApp).size shouldBe 4
    sender.getDataByKey(dApp, "amount_0") shouldBe IntegerDataEntry("amount_0", 3)
    sender.getDataByKey(dApp, "asset_0") shouldBe BinaryDataEntry("asset_0", asset1.id)
    sender.getDataByKey(dApp, "amount_1") shouldBe IntegerDataEntry("amount_1", 6)
    sender.getDataByKey(dApp, "asset_1") shouldBe BinaryDataEntry("asset_1", asset2.id)
  }

  test("can't invoke with three payments") {
    sender.invokeScript(
      caller, dApp, payment = Seq(Payment(3, Waves), Payment(6, Waves), Payment(7, Waves)), waitForTx = true)
    //TODO check error
  }

  test("can't attach more than balance") {
    val wavesBalance = sender.accountBalances(caller)._1
    val asset1Balance = sender.assetBalance(caller, asset1.id.toString).balance

    sender.invokeScript(
      caller,
      dApp,
      payment = Seq(Payment(wavesBalance - 1.waves, Waves), Payment(2.waves, Waves)),
      waitForTx = true)
    //TODO check error

    sender.invokeScript(caller,
      dApp,
      payment = Seq(Payment(asset1Balance - 1000, asset1), Payment(1001, asset1)),
      waitForTx = true)
    //TODO check error
  }

  test("can't attach leased Waves") {
    val wavesBalance = sender.accountBalances(caller)._1
    sender.lease(caller, dApp, wavesBalance - 1.waves, waitForTx = true)
    sender.invokeScript(caller, dApp, payment = Seq(Payment(2.waves, Waves)), waitForTx = true)
    //TODO check error
  }

  test("can't attach with zero Waves amount") {
    sender.invokeScript(caller, dApp, payment = Seq(Payment(1, asset1), Payment(0, Waves)), waitForTx = true)
    //TODO check error
  }

  test("can't attach with zero amount") {
    sender.invokeScript(caller, dApp, payment = Seq(Payment(1, Waves), Payment(0, asset1)), waitForTx = true)
    //TODO check error
  }

  //TODO not existed asset id

}

object InvokeMultiplePaymentsSuite {
  implicit class TypedDataEntry(entry: DataEntry[_]) {
    def as[T]: T = entry.asInstanceOf[T]
  }
}
