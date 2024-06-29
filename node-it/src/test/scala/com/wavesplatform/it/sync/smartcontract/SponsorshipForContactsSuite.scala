package com.wavesplatform.it.sync.smartcontract
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class SponsorshipForContactsSuite extends BaseTransactionSuite with CancelAfterFailure {

  test("sponsor continues to be a sponsor after setScript for account, fee not changed for others") {
    val acc0    = firstKeyPair
    val assetId = sender.issue(firstKeyPair, "asset", "decr", someAssetAmount, 0, reissuable = false, issueFee, 2, None, waitForTx = true).id
    sender.sponsorAsset(firstKeyPair, assetId, 100, sponsorReducedFee, waitForTx = true)
    sender.transfer(firstKeyPair, secondAddress, someAssetAmount / 2, minFee, Some(assetId), None, waitForTx = true)

    val script = ScriptCompiler.compile(s"""false""".stripMargin, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    val _      = sender.setScript(acc0, Some(script), setScriptFee, waitForTx = true)

    val firstAddressBalance       = sender.accountBalances(firstAddress)._1
    val secondAddressBalance      = sender.accountBalances(secondAddress)._1
    val firstAddressAssetBalance  = sender.assetBalance(firstAddress, assetId).balance
    val secondAddressAssetBalance = sender.assetBalance(secondAddress, assetId).balance

    sender.transfer(secondKeyPair, firstAddress, transferAmount, 100, None, Some(assetId), waitForTx = true)

    sender.accountBalances(firstAddress)._1 shouldBe firstAddressBalance + transferAmount - minFee
    sender.accountBalances(secondAddress)._1 shouldBe secondAddressBalance - transferAmount
    sender.assetBalance(firstAddress, assetId).balance shouldBe firstAddressAssetBalance + 100
    sender.assetBalance(secondAddress, assetId).balance shouldBe secondAddressAssetBalance - 100
  }

}
