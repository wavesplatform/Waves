package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.AddressOrAlias
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.state.EitherExt2
import com.wavesplatform.transaction.transfer._
import org.scalatest.CancelAfterFailure
import scala.concurrent.duration._

class TransferTransactionV1Suite extends BaseTransactionSuite with CancelAfterFailure {

  test("asset transfer changes sender's and recipient's asset balance; issuer's.waves balance is decreased by fee") {
    val (firstBalance, firstEffBalance)   = notMiner.accountBalances(firstAddress)
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    val issuedAssetId = sender.issue(firstAddress, "name", "description", someAssetAmount, 2, reissuable = false, issueFee).id

    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

    notMiner.assertBalances(firstAddress, firstBalance - issueFee, firstEffBalance - issueFee)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)

    val transferTransactionId = sender.transfer(firstAddress, secondAddress, someAssetAmount, minFee, Some(issuedAssetId)).id
    nodes.waitForHeightAriseAndTxPresent(transferTransactionId)

    notMiner.assertBalances(firstAddress, firstBalance - minFee - issueFee, firstEffBalance - minFee - issueFee)
    notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, 0)
    notMiner.assertAssetBalance(secondAddress, issuedAssetId, someAssetAmount)
  }

  test("waves transfer changes waves balances and eff.b.") {
    val (firstBalance, firstEffBalance)   = notMiner.accountBalances(firstAddress)
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    val transferId = sender.transfer(firstAddress, secondAddress, transferAmount, minFee).id

    nodes.waitForHeightAriseAndTxPresent(transferId)

    notMiner.assertBalances(firstAddress, firstBalance - transferAmount - minFee, firstEffBalance - transferAmount - minFee)
    notMiner.assertBalances(secondAddress, secondBalance + transferAmount, secondEffBalance + transferAmount)
  }

  test("invalid signed waves transfer should not be in UTX or blockchain") {
    def invalidTx(timestamp: Long = System.currentTimeMillis, fee: Long = 100000): TransferTransactionV1.TransactionT =
      TransferTransactionV1
        .selfSigned(None, sender.privateKey, AddressOrAlias.fromString(sender.address).explicitGet(), 1, timestamp, None, fee, Array.emptyByteArray)
        .right
        .get

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val invalidTxs = Seq(
      (invalidTx(timestamp = System.currentTimeMillis + 1.day.toMillis), "Transaction .* is from far future"),
      (invalidTx(fee = 99999), "Fee .* does not exceed minimal value")
    )

    for ((tx, diag) <- invalidTxs) {
      assertBadRequestAndResponse(sender.broadcastRequest(tx.json()), diag)
      nodes.foreach(_.ensureTxDoesntExist(tx.id().base58))
    }

    nodes.waitForHeightArise()
    notMiner.assertBalances(firstAddress, balance1, eff1)

  }

  test("can not make transfer without having enough effective balance") {
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    assertBadRequest(sender.transfer(secondAddress, firstAddress, secondEffBalance, minFee))
    nodes.waitForHeightArise()

    notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
  }

  test("can not make transfer without having enough balance") {
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    assertBadRequestAndResponse(sender.transfer(secondAddress, firstAddress, secondBalance + 1.waves, minFee),
                                "Attempt to transfer unavailable funds")
    notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
  }

  test("can forge block with sending majority of some asset to self and to other account") {
    val (firstBalance, firstEffBalance)   = notMiner.accountBalances(firstAddress)
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    val assetId = sender.issue(firstAddress, "second asset", "description", someAssetAmount, 0, reissuable = false, fee = issueFee).id

    nodes.waitForHeightAriseAndTxPresent(assetId)

    notMiner.assertBalances(firstAddress, firstBalance - issueFee, firstEffBalance - issueFee)
    notMiner.assertAssetBalance(firstAddress, assetId, someAssetAmount)

    val tx1 = sender.transfer(firstAddress, firstAddress, someAssetAmount, minFee, Some(assetId)).id
    nodes.waitForHeightAriseAndTxPresent(tx1)

    val tx2 = sender.transfer(firstAddress, secondAddress, someAssetAmount / 2, minFee, Some(assetId)).id
    nodes.waitForHeightAriseAndTxPresent(tx2)

    notMiner.assertBalances(firstAddress, firstBalance - issueFee - 2 * minFee, firstEffBalance - issueFee - 2 * minFee)
    notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
  }
}
