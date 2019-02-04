package com.wavesplatform.it.sync.smartcontract
import com.wavesplatform.it.sync._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.transaction.smart.SetScriptTransaction
import org.scalatest.CancelAfterFailure

class SponsorshipForContactsSuite extends BaseTransactionSuite with CancelAfterFailure {

  test("sporsor continues to be a sponsor after setScript for account, fee not changed for others") {
    val acc0    = pkByAddress(firstAddress)
    val assetId = sender.issue(firstAddress, "asset", "decr", someAssetAmount, 0, reissuable = false, issueFee, 2, None, waitForTx = true).id
    sender.sponsorAsset(firstAddress, assetId, 100, sponsorFee, waitForTx = true)
    sender.transfer(firstAddress, secondAddress, someAssetAmount / 2, minFee, Some(assetId), None, waitForTx = true)
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(acc0, Some(script), setScriptFee, System.currentTimeMillis())
      .right
      .get

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json())
      .id

    sender.waitForTransaction(setScriptId)

    val firstAddressBalance       = sender.accountBalances(firstAddress)._1
    val secondAddressBalance      = sender.accountBalances(secondAddress)._1
    val firstAddressAssetBalance  = sender.assetBalance(firstAddress, assetId).balance
    val secondAddressAssetBalance = sender.assetBalance(secondAddress, assetId).balance

    sender.transfer(secondAddress, firstAddress, transferAmount, 100, None, Some(assetId), waitForTx = true)

    sender.accountBalances(firstAddress)._1 shouldBe firstAddressBalance + transferAmount - minFee
    sender.accountBalances(secondAddress)._1 shouldBe secondAddressBalance - transferAmount
    sender.assetBalance(firstAddress, assetId).balance shouldBe firstAddressAssetBalance + 100
    sender.assetBalance(secondAddress, assetId).balance shouldBe secondAddressAssetBalance - 100
  }

}
