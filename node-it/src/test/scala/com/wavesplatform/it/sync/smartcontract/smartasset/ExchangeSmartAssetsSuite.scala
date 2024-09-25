package com.wavesplatform.it.sync.smartcontract.smartasset

import com.wavesplatform.api.http.ApiError.TransactionNotAllowedByAssetScript
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.smartcontract.{cryptoContextScript, pureContextScript, wavesContextScript, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.DataTransaction
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class ExchangeSmartAssetsSuite extends BaseTransactionSuite with CancelAfterFailure with NTPTime {
  private val estimator = ScriptEstimatorV2

  private def acc0 = firstKeyPair
  private def acc1 = secondKeyPair
  private def acc2 = thirdKeyPair

  private var dtx: DataTransaction = _

  private val sc1 = Some("true")

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", value = true)
    val entry3 = BinaryDataEntry("blob", ByteStr.decodeBase64("YWxpY2U=").get)
    val entry4 = StringDataEntry("str", "test")

    dtx = DataTransaction.selfSigned(1.toByte, acc0, List(entry1, entry2, entry3, entry4), minFee, ntpTime.correctedTime()).explicitGet()
    sender.signedBroadcast(dtx.json(), waitForTx = true)
  }

  test("require using a certain matcher with smart accounts") {
    /*
    combination of smart accounts and smart assets
     */
    val s = Some(
      ScriptCompiler
        .compile(
          s"""{-# SCRIPT_TYPE ASSET #-}
             |match tx {
             |case _: SetAssetScriptTransaction => true
             |case e: ExchangeTransaction => e.sender == addressFromPublicKey(base58'${acc2.publicKey}')
             |case _ => false}""".stripMargin,
          estimator
        )
        .explicitGet()
        ._1
        .bytes()
        .base64
    )

    val sAsset = sender
      .issue(firstKeyPair, "SmartAsset", "TestCoin", someAssetAmount, 0, reissuable = false, issueFee, 2, s, waitForTx = true)
      .id

    val smartPair = AssetPair(IssuedAsset(ByteStr.decodeBase58(sAsset).get), Waves)

    for (
      (contr1, contr2, mcontr) <- Seq(
        (sc1, sc1, sc1),
        (None, sc1, None)
      )
    ) {

      setContracts((contr1, acc0), (contr2, acc1), (mcontr, acc2))

      sender.signedBroadcast(
        exchangeTx(smartPair, smartMatcherFee + smartFee, smartMatcherFee + smartFee, ntpTime, 2, 3, acc1, acc0, acc2),
        waitForTx = true
      )
    }

    val sUpdated = Some(
      ScriptCompiler
        .compile(
          s"""{-# SCRIPT_TYPE ASSET #-}
             |match tx {
             |case _: SetAssetScriptTransaction => true
             |case e: ExchangeTransaction => e.sender == addressFromPublicKey(base58'${acc1.publicKey}')
             |case _ => false}""".stripMargin,
          estimator
        )
        .explicitGet()
        ._1
        .bytes()
        .base64
    )

    sender.setAssetScript(sAsset, firstKeyPair, setAssetScriptFee, sUpdated, waitForTx = true)

    assertApiError(
      sender.signedBroadcast(exchangeTx(smartPair, smartMatcherFee + smartFee, smartMatcherFee + smartFee, ntpTime, 3, 2, acc1, acc0, acc2)),
      AssertiveApiError(TransactionNotAllowedByAssetScript.Id, "Transaction is not allowed by token-script")
    )

    setContracts((None, acc0), (None, acc1), (None, acc2))
  }

  test("AssetPair from smart assets") {
    val assetA = sender
      .issue(firstKeyPair, "assetA", "TestCoin", someAssetAmount, 0, reissuable = false, issueFee, 2, Some(scriptBase64), waitForTx = true)
      .id

    val assetB = sender
      .issue(secondKeyPair, "assetB", "TestCoin", someAssetAmount, 0, reissuable = false, issueFee, 2, Some(scriptBase64), waitForTx = true)
      .id

    sender.transfer(secondKeyPair, firstAddress, 1000, minFee + smartFee, Some(assetB), waitForTx = true)
    sender.transfer(firstKeyPair, secondAddress, 1000, minFee + smartFee, Some(assetA), waitForTx = true)

    val script = Some(
      ScriptCompiler
        .compile(
          s"""{-# SCRIPT_TYPE ASSET #-}
             |let assetA = base58'$assetA'
             |let assetB = base58'$assetB'
             |match tx {
             |case _: SetAssetScriptTransaction => true
             |case e: ExchangeTransaction => (e.sellOrder.assetPair.priceAsset == assetA || e.sellOrder.assetPair.amountAsset == assetA) && (e.sellOrder.assetPair.priceAsset == assetB || e.sellOrder.assetPair.amountAsset == assetB)
             |case _ => false}""".stripMargin,
          estimator
        )
        .explicitGet()
        ._1
        .bytes()
        .base64
    )

    sender.setAssetScript(assetA, firstKeyPair, setAssetScriptFee, script, waitForTx = true)
    sender.setAssetScript(assetB, secondKeyPair, setAssetScriptFee, script, waitForTx = true)

    val smartAssetPair = AssetPair(
      amountAsset = IssuedAsset(ByteStr.decodeBase58(assetA).get),
      priceAsset = IssuedAsset(ByteStr.decodeBase58(assetB).get)
    )

    sender.signedBroadcast(
      exchangeTx(smartAssetPair, matcherFee + 2 * smartFee, matcherFee + 2 * smartFee, ntpTime, 3, 2, acc1, acc0, acc2),
      waitForTx = true
    )

    withClue("check fee for smart accounts and smart AssetPair - extx.fee == 0.015.waves") {
      setContracts((sc1, acc0), (sc1, acc1), (sc1, acc2))

      assertBadRequestAndMessage(
        sender.signedBroadcast(exchangeTx(smartAssetPair, smartMatcherFee + smartFee, smartMatcherFee + smartFee, ntpTime, 2, 2, acc1, acc0, acc2)),
        "does not exceed minimal value of 1500000 WAVES"
      )

      sender.signedBroadcast(
        exchangeTx(smartAssetPair, smartMatcherFee + 2 * smartFee, smartMatcherFee + 2 * smartFee, ntpTime, 2, 3, acc1, acc0, acc2),
        waitForTx = true
      )
      setContracts((None, acc0), (None, acc1), (None, acc2))
    }

    withClue("try to use incorrect assetPair") {
      val incorrectSmartAssetPair = AssetPair(
        amountAsset = IssuedAsset(ByteStr.decodeBase58(assetA).get),
        priceAsset = Waves
      )

      assertApiError(
        sender
          .signedBroadcast(exchangeTx(incorrectSmartAssetPair, smartMatcherFee, smartMatcherFee, ntpTime, 3, 2, acc1, acc0, acc2)),
        AssertiveApiError(TransactionNotAllowedByAssetScript.Id, "Transaction is not allowed by token-script")
      )
    }
  }

  test("use all functions from RIDE for asset script") {
    val script1 = Some(ScriptCompiler.compile("{-# SCRIPT_TYPE ASSET #-}" + cryptoContextScript(false), estimator).explicitGet()._1.bytes().base64)
    val script2 = Some(ScriptCompiler.compile("{-# SCRIPT_TYPE ASSET #-}" + pureContextScript(dtx, false), estimator).explicitGet()._1.bytes().base64)
    val script3 =
      Some(ScriptCompiler.compile("{-# SCRIPT_TYPE ASSET #-}" + wavesContextScript(dtx, false), estimator).explicitGet()._1.bytes().base64)

    List(script1, script2, script3)
      .map { i =>
        val asset = sender
          .issue(firstKeyPair, "assetA", "TestCoin", someAssetAmount, 0, reissuable = false, issueFee, 2, i, waitForTx = true)
          .id

        val smartPair = AssetPair(IssuedAsset(ByteStr.decodeBase58(asset).get), Waves)

        sender.signedBroadcast(exchangeTx(smartPair, smartMatcherFee, smartMatcherFee, ntpTime, 2, 3, acc1, acc0, acc2), waitForTx = true)
      }
  }
}
