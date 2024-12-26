package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.{AddressOrAlias, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.api.TransferTransactionInfo
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.transfer.TransferTransaction.MaxAttachmentSize
import com.wavesplatform.transaction.{Proofs, TxPositiveAmount, TxVersion, TransactionSignOps}
import org.scalatest.CancelAfterFailure
import play.api.libs.json.Json

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.*

class TransferTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  test("transfer with empty string assetId") {
    val tx = TransferTransaction
      .selfSigned(2.toByte, sender.keyPair, sender.keyPair.toAddress, Waves, 100L, Waves, minFee, ByteStr.empty, System.currentTimeMillis())
      .explicitGet()
    val json = tx.json() ++ Json.obj("assetId" -> "", "feeAssetId" -> "")
    sender.signedBroadcast(json, waitForTx = true)
  }

  test("asset transfer changes sender's and recipient's asset balance; issuer's.waves balance is decreased by fee") {
    for (v <- transferTxSupportedVersions) {
      val (firstBalance, firstEffBalance)   = miner.accountBalances(firstAddress)
      val (secondBalance, secondEffBalance) = miner.accountBalances(secondAddress)

      val issuedAssetId = sender.issue(firstKeyPair, "name", "description", someAssetAmount, 2, reissuable = false, issueFee).id

      nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

      miner.assertBalances(firstAddress, firstBalance - issueFee, firstEffBalance - issueFee)
      miner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)

      val transferTransaction = sender.transfer(firstKeyPair, secondAddress, someAssetAmount, minFee, Some(issuedAssetId), version = v)
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
      val (firstBalance, firstEffBalance)   = miner.accountBalances(firstAddress)
      val (secondBalance, secondEffBalance) = miner.accountBalances(secondAddress)

      val transferId = sender.transfer(firstKeyPair, secondAddress, transferAmount, minFee, version = v).id

      nodes.waitForHeightAriseAndTxPresent(transferId)

      miner.assertBalances(firstAddress, firstBalance - transferAmount - minFee, firstEffBalance - transferAmount - minFee)
      miner.assertBalances(secondAddress, secondBalance + transferAmount, secondEffBalance + transferAmount)
    }
  }

  test("invalid signed waves transfer should not be in UTX or blockchain") {
    def invalidTx(
        version: TxVersion,
        timestamp: Long = System.currentTimeMillis,
        fee: Long = 100000,
        attachment: Array[Byte] = Array.emptyByteArray
    ): TransferTransaction = {
      val tx = TransferTransaction(
        version = version,
        sender = sender.keyPair.publicKey,
        recipient = AddressOrAlias.fromString(sender.address).explicitGet(),
        assetId = Waves,
        amount = TxPositiveAmount.unsafeFrom(1),
        feeAssetId = Waves,
        fee = TxPositiveAmount.unsafeFrom(fee),
        attachment = ByteStr(attachment),
        timestamp = timestamp,
        proofs = Proofs.empty,
        chainId = AddressScheme.current.chainId
      )

      tx.signWith(sender.keyPair.privateKey)
    }

    val (balance1, eff1) = miner.accountBalances(firstAddress)

    val invalidTxs = for {
      v <- transferTxSupportedVersions
      x <- Seq(
        (invalidTx(v, timestamp = System.currentTimeMillis + 1.day.toMillis), "Transaction timestamp .* is more than .*ms in the future"),
        (invalidTx(v, fee = 99999), "Fee .* does not exceed minimal value"),
        (invalidTx(v, attachment = ("1" * (MaxAttachmentSize + 1)).getBytes(StandardCharsets.UTF_8)), "exceeds maximum length"),
        (
          invalidTx(v, attachment = Array.fill(MaxAttachmentSize + 1)(1)),
          "Invalid attachment. Length \\d+ bytes exceeds maximum of \\d+ bytes."
        )
      )
    } yield x

    for ((tx, diag) <- invalidTxs) {
      assertBadRequestAndResponse(sender.broadcastRequest(tx.json()), diag)
      nodes.foreach(_.ensureTxDoesntExist(tx.id().toString))
    }

    nodes.waitForHeightArise()
    miner.assertBalances(firstAddress, balance1, eff1)

  }

  test("can not make transfer without having enough effective balance") {
    for (v <- transferTxSupportedVersions) {
      val (secondBalance, secondEffBalance) = miner.accountBalances(secondAddress)

      assertApiErrorRaised(sender.transfer(secondKeyPair, firstAddress, secondEffBalance, minFee, version = v))
      nodes.waitForHeightArise()

      miner.assertBalances(secondAddress, secondBalance, secondEffBalance)
    }
  }

  test("can not make transfer without having enough balance") {
    for (v <- transferTxSupportedVersions) {
      val (secondBalance, secondEffBalance) = miner.accountBalances(secondAddress)

      assertBadRequestAndResponse(
        sender.transfer(secondKeyPair, firstAddress, secondBalance + 1.waves, minFee, version = v),
        "Attempt to transfer unavailable funds"
      )
      miner.assertBalances(secondAddress, secondBalance, secondEffBalance)
    }
  }

  test("can forge block with sending majority of some asset to self and to other account") {
    for (v <- transferTxSupportedVersions) {
      val (firstBalance, firstEffBalance)   = miner.accountBalances(firstAddress)
      val (secondBalance, secondEffBalance) = miner.accountBalances(secondAddress)

      val assetId = sender.issue(firstKeyPair, "second asset", "description", someAssetAmount, 0, reissuable = false, fee = issueFee).id

      nodes.waitForHeightAriseAndTxPresent(assetId)

      miner.assertBalances(firstAddress, firstBalance - issueFee, firstEffBalance - issueFee)
      miner.assertAssetBalance(firstAddress, assetId, someAssetAmount)

      val tx1 = sender.transfer(firstKeyPair, firstAddress, someAssetAmount, minFee, Some(assetId), version = v).id
      nodes.waitForHeightAriseAndTxPresent(tx1)

      val tx2 = sender.transfer(firstKeyPair, secondAddress, someAssetAmount / 2, minFee, Some(assetId), version = v).id
      nodes.waitForHeightAriseAndTxPresent(tx2)

      miner.assertBalances(firstAddress, firstBalance - issueFee - 2 * minFee, firstEffBalance - issueFee - 2 * minFee)
      miner.assertBalances(secondAddress, secondBalance, secondEffBalance)
    }
  }
}
