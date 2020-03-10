package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.{AddressOrAlias, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{Block, Transaction, TransferTransactionInfo}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.transfer.Attachment.Bin
import com.wavesplatform.transaction.transfer._
import org.scalatest.{Assertion, Assertions, CancelAfterFailure}
import play.api.libs.json.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, Json}

import scala.concurrent.duration._

class TransferTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  test("asset transfer changes sender's and recipient's asset balance; issuer's.waves balance is decreased by fee") {
    for (v <- transferTxSupportedVersions) {
      val (firstBalance, firstEffBalance)   = miner.accountBalances(firstAddress)
      val (secondBalance, secondEffBalance) = miner.accountBalances(secondAddress)

      val issuedAssetId = sender.issue(firstAddress, "name", "description", someAssetAmount, 2, reissuable = false, issueFee).id

      nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

      miner.assertBalances(firstAddress, firstBalance - issueFee, firstEffBalance - issueFee)
      miner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)

      val transferTransaction = sender.transfer(firstAddress, secondAddress, someAssetAmount, minFee, Some(issuedAssetId), version = v)
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

      val transferId = sender.transfer(firstAddress, secondAddress, transferAmount, minFee, version = v).id

      nodes.waitForHeightAriseAndTxPresent(transferId)

      miner.assertBalances(firstAddress, firstBalance - transferAmount - minFee, firstEffBalance - transferAmount - minFee)
      miner.assertBalances(secondAddress, secondBalance + transferAmount, secondEffBalance + transferAmount)
    }
  }

  test("invalid signed waves transfer should not be in UTX or blockchain") {
    def invalidTx(timestamp: Long = System.currentTimeMillis, fee: Long = 100000): TransferTransaction =
      TransferTransaction
        .selfSigned(1.toByte, sender.privateKey, AddressOrAlias.fromString(sender.address).explicitGet(), Waves, 1, Waves, fee, None, timestamp)
        .right
        .get

    val (balance1, eff1) = miner.accountBalances(firstAddress)

    val invalidTxs = Seq(
      (invalidTx(timestamp = System.currentTimeMillis + 1.day.toMillis), "Transaction timestamp .* is more than .*ms in the future"),
      (invalidTx(fee = 99999), "Fee .* does not exceed minimal value")
    )

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

      assertApiErrorRaised(sender.transfer(secondAddress, firstAddress, secondEffBalance, minFee, version = v))
      nodes.waitForHeightArise()

      miner.assertBalances(secondAddress, secondBalance, secondEffBalance)
    }
  }

  test("can not make transfer without having enough balance") {
    for (v <- transferTxSupportedVersions) {
      val (secondBalance, secondEffBalance) = miner.accountBalances(secondAddress)

      assertBadRequestAndResponse(
        sender.transfer(secondAddress, firstAddress, secondBalance + 1.waves, minFee, version = v),
        "Attempt to transfer unavailable funds"
      )
      miner.assertBalances(secondAddress, secondBalance, secondEffBalance)
    }
  }

  test("can forge block with sending majority of some asset to self and to other account") {
    for (v <- transferTxSupportedVersions) {
      val (firstBalance, firstEffBalance)   = miner.accountBalances(firstAddress)
      val (secondBalance, secondEffBalance) = miner.accountBalances(secondAddress)

      val assetId = sender.issue(firstAddress, "second asset", "description", someAssetAmount, 0, reissuable = false, fee = issueFee).id

      nodes.waitForHeightAriseAndTxPresent(assetId)

      miner.assertBalances(firstAddress, firstBalance - issueFee, firstEffBalance - issueFee)
      miner.assertAssetBalance(firstAddress, assetId, someAssetAmount)

      val tx1 = sender.transfer(firstAddress, firstAddress, someAssetAmount, minFee, Some(assetId), version = v).id
      nodes.waitForHeightAriseAndTxPresent(tx1)

      val tx2 = sender.transfer(firstAddress, secondAddress, someAssetAmount / 2, minFee, Some(assetId), version = v).id
      nodes.waitForHeightAriseAndTxPresent(tx2)

      miner.assertBalances(firstAddress, firstBalance - issueFee - 2 * minFee, firstEffBalance - issueFee - 2 * minFee)
      miner.assertBalances(secondAddress, secondBalance, secondEffBalance)
    }
  }

  test("able to pass typed attachment to transfer transaction V3") {

    val txWithStringAtt =
      sender.transfer(
        firstAddress,
        secondAddress,
        transferAmount,
        minFee,
        version = TxVersion.V3,
        typedAttachment = Some(Attachment.Str("somestring")),
        waitForTx = true
      )
    val txWithStringAttInfo = sender.transactionInfo[TransferTransactionInfo](txWithStringAtt.id)
    txWithStringAttInfo.typedAttachment shouldBe Some(Attachment.Str("somestring"))

    val txWithBoolAtt =
      sender.transfer(
        firstAddress,
        secondAddress,
        transferAmount,
        minFee,
        version = TxVersion.V3,
        typedAttachment = Some(Attachment.Bool(false)),
        waitForTx = true
      )
    val txWithBoolAttInfo = sender.transactionInfo[TransferTransactionInfo](txWithBoolAtt.id)
    txWithBoolAttInfo.typedAttachment shouldBe Some(Attachment.Bool(false))

    val txWithIntAtt =
      sender.transfer(
        firstAddress,
        secondAddress,
        transferAmount,
        minFee,
        version = TxVersion.V3,
        typedAttachment = Some(Attachment.Num(123)),
        waitForTx = true
      )
    val txWithIntAttInfo = sender.transactionInfo[TransferTransactionInfo](txWithIntAtt.id)
    txWithIntAttInfo.typedAttachment shouldBe Some(Attachment.Num(123))

    val txWithBinaryAtt =
      sender.transfer(
        firstAddress,
        secondAddress,
        transferAmount,
        minFee,
        version = TxVersion.V3,
        typedAttachment = Some(Attachment.Bin(Array[Byte](127.toByte, 0, 1, 1))),
        waitForTx = true
      )
    val txWithBinaryAttInfo = sender.transactionInfo[TransferTransactionInfo](txWithBinaryAtt.id)
    txWithBinaryAttInfo.typedAttachment.get.asInstanceOf[Bin].value shouldBe Attachment.Bin(Array[Byte](127.toByte, 0, 1, 1)).value
  }

  test("not able to pass typed attachment to transfer transaction V1,2") {
    for (v <- transferTxSupportedVersions if v < 3) {
      assertApiError(
        sender.transfer(
          firstAddress,
          secondAddress,
          transferAmount,
          minFee,
          version = v,
          typedAttachment = Some(Attachment.Num(123))
        )
      ) { error =>
        error.id shouldBe 10
        error.message shouldBe "Too big sequences requested"
      }
    }
  }
}
