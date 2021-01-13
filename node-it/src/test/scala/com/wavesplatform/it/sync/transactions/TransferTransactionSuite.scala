package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.{AddressOrAlias, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{BalanceDetails, TransferTransactionInfo}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer._
import play.api.libs.json.Json

import scala.concurrent.duration._

class TransferTransactionSuite extends BaseTransactionSuite {
  test("transfer with empty string assetId") {
    val tx = TransferTransaction.selfSigned(2.toByte, miner.keyPair, miner.keyPair.toAddress, Waves, 100L, Waves, minFee, ByteStr.empty, System.currentTimeMillis()).explicitGet()
    val json = tx.json() ++ Json.obj("assetId" -> "", "feeAssetId" -> "")
    miner.signedBroadcast(json, waitForTx = true)
  }

  test("asset transfer changes sender's and recipient's asset balance; issuer's.waves balance is decreased by fee") {
    for (v <- transferTxSupportedVersions) {
      val BalanceDetails(_, firstBalance, _, _, firstEffBalance) = miner.balanceDetails(firstAddress)
      val BalanceDetails(_, secondBalance, _, _, secondEffBalance) = miner.balanceDetails(secondAddress)

      val issuedAssetId = miner.issue(firstKeyPair, "name", "description", someAssetAmount, 2, reissuable = false, issueFee).id

      nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

      miner.assertBalances(firstAddress, firstBalance - issueFee, firstEffBalance - issueFee)
      miner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)

      val transferTransaction = miner.transfer(firstKeyPair, secondAddress, someAssetAmount, minFee, Some(issuedAssetId), version = v)
      nodes.waitForHeightAriseAndTxPresent(transferTransaction.id)
      if (v > 2) {
        transferTransaction.chainId shouldBe Some(AddressScheme.current.chainId)
        miner.transactionInfo[TransferTransactionInfo](transferTransaction.id).chainId shouldBe Some(AddressScheme.current.chainId)
      }

      miner.assertBalances(firstAddress, firstBalance - minFee - issueFee, firstEffBalance - minFee - issueFee)
      miner.assertBalances(secondAddress, secondBalance, secondEffBalance)
      miner.assertAssetBalance(firstAddress, issuedAssetId, 0)
      miner.assertAssetBalance(secondAddress, issuedAssetId, someAssetAmount)
    }
  }

  test("waves transfer changes waves balances and eff.b.") {
    for (v <- transferTxSupportedVersions) {
      val BalanceDetails(_, firstBalance, _, _, firstEffBalance)   = miner.balanceDetails(firstAddress)
      val BalanceDetails(_, secondBalance, _, _, secondEffBalance) = miner.balanceDetails(secondAddress)

      val transferId = miner.transfer(firstKeyPair, secondAddress, transferAmount, minFee, version = v).id

      nodes.waitForHeightAriseAndTxPresent(transferId)

      miner.assertBalances(firstAddress, firstBalance - transferAmount - minFee, firstEffBalance - transferAmount - minFee)
      miner.assertBalances(secondAddress, secondBalance + transferAmount, secondEffBalance + transferAmount)
    }
  }

  test("invalid signed waves transfer should not be in UTX or blockchain") {
    def invalidTx(timestamp: Long = System.currentTimeMillis, fee: Long = 100000): TransferTransaction =
      TransferTransaction
        .selfSigned(1.toByte, miner.keyPair, AddressOrAlias.fromString(miner.address).explicitGet(), Waves, 1, Waves, fee, ByteStr.empty, timestamp)
        .explicitGet()

    val BalanceDetails(_, balance1, _, _, eff1) = miner.balanceDetails(firstAddress)

    val invalidTxs = Seq(
      (invalidTx(timestamp = System.currentTimeMillis + 1.day.toMillis), "Transaction timestamp .* is more than .*ms in the future"),
      (invalidTx(fee = 99999), "Fee .* does not exceed minimal value")
    )

    for ((tx, diag) <- invalidTxs) {
      assertBadRequestAndResponse(miner.broadcastRequest(tx.json()), diag)
      nodes.foreach(_.ensureTxDoesntExist(tx.id().toString))
    }

    nodes.waitForHeightArise()
    miner.assertBalances(firstAddress, balance1, eff1)

  }

  test("can not make transfer without having enough effective balance") {
    for (v <- transferTxSupportedVersions) {
      val BalanceDetails(_, secondBalance, _, _, secondEffBalance) = miner.balanceDetails(secondAddress)

      assertApiErrorRaised(miner.transfer(secondKeyPair, firstAddress, secondEffBalance, minFee, version = v))
      nodes.waitForHeightArise()

      miner.assertBalances(secondAddress, secondBalance, secondEffBalance)
    }
  }

  test("can not make transfer without having enough balance") {
    for (v <- transferTxSupportedVersions) {
      val BalanceDetails(_, secondBalance, _, _, secondEffBalance) = miner.balanceDetails(secondAddress)

      assertBadRequestAndResponse(
        miner.transfer(secondKeyPair, firstAddress, secondBalance + 1.waves, minFee, version = v),
        "Attempt to transfer unavailable funds"
      )
      miner.assertBalances(secondAddress, secondBalance, secondEffBalance)
    }
  }

  test("can forge block with sending majority of some asset to self and to other account") {
    for (v <- transferTxSupportedVersions) {
      val BalanceDetails(_, firstBalance, _, _, firstEffBalance)   = miner.balanceDetails(firstAddress)
      val BalanceDetails(_, secondBalance, _, _, secondEffBalance) = miner.balanceDetails(secondAddress)

      val assetId = miner.issue(firstKeyPair, "second asset", "description", someAssetAmount, 0, reissuable = false, fee = issueFee).id

      nodes.waitForHeightAriseAndTxPresent(assetId)

      miner.assertBalances(firstAddress, firstBalance - issueFee, firstEffBalance - issueFee)
      miner.assertAssetBalance(firstAddress, assetId, someAssetAmount)

      val tx1 = miner.transfer(firstKeyPair, firstAddress, someAssetAmount, minFee, Some(assetId), version = v).id
      nodes.waitForHeightAriseAndTxPresent(tx1)

      val tx2 = miner.transfer(firstKeyPair, secondAddress, someAssetAmount / 2, minFee, Some(assetId), version = v).id
      nodes.waitForHeightAriseAndTxPresent(tx2)

      miner.assertBalances(firstAddress, firstBalance - issueFee - 2 * minFee, firstEffBalance - issueFee - 2 * minFee)
      miner.assertBalances(secondAddress, secondBalance, secondEffBalance)
    }
  }
}
