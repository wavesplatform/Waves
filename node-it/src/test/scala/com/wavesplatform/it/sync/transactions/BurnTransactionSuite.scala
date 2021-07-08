package com.wavesplatform.it.sync.transactions

import cats.syntax.option._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.it.api.BurnTransactionInfo
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{issueAmount, issueFee, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.BurnTransaction
import play.api.libs.json.Json

class BurnTransactionSuite extends BaseTransactionSuite {

  private val decimals: Byte = 2

  test("burning assets changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    for (v <- burnTxSupportedVersions) {
      val (balance, effectiveBalance) = miner.accountBalances(firstAddress)
      val issuedAssetId =
        sender.issue(firstKeyPair, s"name+$v", "description", issueAmount, decimals, reissuable = false, fee = issueFee, waitForTx = true).id

      miner.assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee)
      miner.assertAssetBalance(firstAddress, issuedAssetId, issueAmount)
      val details1 = miner.assetsDetails(issuedAssetId)
      assert(!details1.reissuable)
      assert(details1.quantity == issueAmount)
      assert(details1.minSponsoredAssetFee.isEmpty)

      // burn half of the coins and check balance
      val burnTx = sender.burn(firstKeyPair, issuedAssetId, issueAmount / 2, minFee, version = v, waitForTx = true)

      if (v > 2) {
        burnTx.chainId shouldBe Some(AddressScheme.current.chainId)
        sender.transactionInfo[BurnTransactionInfo](burnTx.id).chainId shouldBe Some(AddressScheme.current.chainId)
      }
      sender.transactionInfo[BurnTransactionInfo](burnTx.id).amount shouldBe issueAmount / 2
      miner.assertBalances(firstAddress, balance - minFee - issueFee, effectiveBalance - minFee - issueFee)
      miner.assertAssetBalance(firstAddress, issuedAssetId, issueAmount / 2)
      val details2 = miner.assetsDetails(issuedAssetId)
      assert(!details2.reissuable)
      assert(details2.quantity == issueAmount - issueAmount / 2)

      val assetOpt = miner.assetsBalance(firstAddress).balances.find(_.assetId == issuedAssetId)
      assert(assetOpt.exists(_.balance == issueAmount / 2))

      // burn the rest and check again
      sender.burn(firstKeyPair, issuedAssetId, issueAmount / 2, minFee, version = v, waitForTx = true).id
      miner.assertAssetBalance(firstAddress, issuedAssetId, 0)
      val details3 = miner.assetsDetails(issuedAssetId)
      assert(!details3.reissuable)
      assert(details3.quantity == 0)
      assert(details1.minSponsoredAssetFee.isEmpty)

      val assetOptRest = miner.assetsBalance(firstAddress).balances.find(_.assetId == issuedAssetId)
      assert(assetOptRest.isEmpty)
    }

    miner
      .transactionsByAddress(firstAddress, limit = 100)
      .count(_._type == BurnTransaction.typeId) shouldBe burnTxSupportedVersions.length * 2
  }

  test("can burn non-owned asset; issuer asset balance decreased by transfer amount; burner balance decreased by burned amount") {
    for (v <- burnTxSupportedVersions) {
      val issuedQuantity      = issueAmount
      val transferredQuantity = issuedQuantity / 2

      val issuedAssetId =
        sender.issue(firstKeyPair, s"name+$v", "description", issuedQuantity, decimals, reissuable = false, issueFee, waitForTx = true).id

      sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity)
      sender.transfer(firstKeyPair, secondAddress, transferredQuantity, minFee, issuedAssetId.some, waitForTx = true).id

      sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity - transferredQuantity)
      sender.assertAssetBalance(secondAddress, issuedAssetId, transferredQuantity)

      sender.burn(secondKeyPair, issuedAssetId, transferredQuantity, minFee, v, waitForTx = true).id
      sender.assertAssetBalance(secondAddress, issuedAssetId, 0)

      val details = miner.assetsDetails(issuedAssetId)
      assert(!details.reissuable)
      assert(details.quantity == issuedQuantity - transferredQuantity)
      assert(details.minSponsoredAssetFee.isEmpty)

      assertBadRequestAndMessage(
        sender.transfer(secondKeyPair, firstAddress, transferredQuantity / 2, minFee, issuedAssetId.some).id,
        "Attempt to transfer unavailable funds"
      )
    }
  }

  test("issuer can't burn more tokens than he own") {
    for (v <- burnTxSupportedVersions) {
      val issuedQuantity = issueAmount
      val burnedQuantity = issuedQuantity * 2

      val issuedAssetId =
        sender.issue(firstKeyPair, s"name+$v", "description", issuedQuantity, decimals, reissuable = false, issueFee).id

      nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

      sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity)
      assertBadRequestAndMessage(sender.burn(secondKeyPair, issuedAssetId, burnedQuantity, minFee, v).id, "Accounts balance errors")
    }
  }

  test("user can't burn more tokens than he own") {
    for (v <- burnTxSupportedVersions) {
      val issuedQuantity      = issueAmount
      val transferredQuantity = issuedQuantity / 2
      val burnedQuantity      = transferredQuantity * 2

      val issuedAssetId = sender.issue(firstKeyPair, s"name+$v", "description", issuedQuantity, decimals, reissuable = false, issueFee).id

      miner.waitForTransaction(issuedAssetId)
      sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity)

      val transferId = sender.transfer(firstKeyPair, secondAddress, transferredQuantity, minFee, issuedAssetId.some).id

      miner.waitForTransaction(transferId)
      sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity - transferredQuantity)
      sender.assertAssetBalance(secondAddress, issuedAssetId, transferredQuantity)

      assertBadRequestAndMessage(sender.burn(secondKeyPair, issuedAssetId, burnedQuantity, minFee, v).id, "Accounts balance errors")
    }
  }

  test("non-owner can burn asset after reissue") {
    for (v <- burnTxSupportedVersions) {
      val issuedQuantity      = issueAmount
      val transferredQuantity = issuedQuantity / 2

      val issuedAssetId = sender.issue(firstKeyPair, s"name+$v", "description", issuedQuantity, decimals, reissuable = true, issueFee).id

      miner.waitForTransaction(issuedAssetId)
      sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity)

      val transferId = sender.transfer(firstKeyPair, secondAddress, transferredQuantity, minFee, issuedAssetId.some).id
      miner.waitForTransaction(transferId)

      val burnOwnerTxTd = sender.burn(firstKeyPair, issuedAssetId, transferredQuantity, minFee, v).id
      miner.waitForTransaction(burnOwnerTxTd)

      sender.assertAssetBalance(firstAddress, issuedAssetId, 0)
      sender.assertAssetBalance(secondAddress, issuedAssetId, transferredQuantity)

      val details = miner.assetsDetails(issuedAssetId)
      assert(details.reissuable)
      assert(details.quantity == transferredQuantity)
      assert(details.minSponsoredAssetFee.isEmpty)

      val reissueId = sender.reissue(firstKeyPair, issuedAssetId, issuedQuantity, false, issueFee).id
      miner.waitForTransaction(reissueId)

      val details1 = miner.assetsDetails(issuedAssetId)
      assert(!details1.reissuable)
      assert(details1.quantity == transferredQuantity + issuedQuantity)
      assert(details1.minSponsoredAssetFee.isEmpty)

      val burn1 = sender.burn(firstKeyPair, issuedAssetId, issuedQuantity, minFee, v).id
      miner.waitForTransaction(burn1)

      val burn2 = sender.burn(secondKeyPair, issuedAssetId, transferredQuantity, minFee, v).id
      miner.waitForTransaction(burn2)

      val details2 = miner.assetsDetails(issuedAssetId)
      assert(!details2.reissuable)
      assert(details2.quantity == 0)
      assert(details2.minSponsoredAssetFee.isEmpty)

      assertBadRequestAndMessage(sender.reissue(firstKeyPair, issuedAssetId, issuedQuantity, true, issueFee).id, "Asset is not reissuable")
      assertBadRequestAndMessage(
        sender.transfer(secondKeyPair, thirdAddress, transferredQuantity / 2, minFee, issuedAssetId.some).id,
        "Attempt to transfer unavailable funds"
      )
      assertBadRequestAndMessage(
        sender.transfer(firstKeyPair, thirdAddress, transferredQuantity / 2, minFee, issuedAssetId.some).id,
        "Attempt to transfer unavailable funds"
      )

    }
  }

  test("send burn with quantity field") {
    val issuedAssetId =
      sender.issue(firstKeyPair, "name", "description", issueAmount, decimals, reissuable = false, fee = issueFee, waitForTx = true).id

    val tx = BurnTransaction
      .selfSigned(TxVersion.V1, firstKeyPair, IssuedAsset(ByteStr.decodeBase58(issuedAssetId).get), 1, minFee, System.currentTimeMillis())
      .explicitGet()
    val json = tx.json() - "amount" ++ Json.obj("quantity" -> 1L)
    sender.signedBroadcast(json, waitForTx = true).id
  }
}
