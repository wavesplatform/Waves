package com.wavesplatform.it.sync.smartcontract
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class SponsorshipForContactsSuite extends BaseTransactionSuite {

  test("sponsor continues to be a sponsor after setScript for account, fee not changed for others") {
    val acc0 = firstKeyPair
    val assetId = miner.issue(firstKeyPair, "asset", "decr", someAssetAmount, 0, reissuable = false, issueFee, 2, None, waitForTx = true).id
    miner.sponsorAsset(firstKeyPair, assetId, 100, sponsorReducedFee, waitForTx = true)
    miner.transfer(firstKeyPair, secondAddress, someAssetAmount / 2, minFee, Some(assetId), None, waitForTx = true)

    val script = ScriptCompiler(s"""false""".stripMargin, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    val _ = miner.setScript(acc0, Some(script), setScriptFee, waitForTx = true)

    val firstAddressBalance       = miner.wavesBalance(firstAddress)
    val secondAddressBalance      = miner.wavesBalance(secondAddress)
    val firstAddressAssetBalance  = miner.assetBalance(firstAddress, assetId).balance
    val secondAddressAssetBalance = miner.assetBalance(secondAddress, assetId).balance

    miner.transfer(secondKeyPair, firstAddress, transferAmount, 100, None, Some(assetId), waitForTx = true)

    miner.wavesBalance(firstAddress) shouldBe firstAddressBalance + transferAmount - minFee
    miner.wavesBalance(secondAddress) shouldBe secondAddressBalance - transferAmount
    miner.assetBalance(firstAddress, assetId).balance shouldBe firstAddressAssetBalance + 100
    miner.assetBalance(secondAddress, assetId).balance shouldBe secondAddressAssetBalance - 100
  }

}
