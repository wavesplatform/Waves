package com.wavesplatform.it.transactions

import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import scorex.account.AddressOrAlias
import scorex.api.http.assets.SignedMassTransferRequest
import scorex.crypto.encode.Base58
import scorex.transaction.assets.MassTransferTransaction
import scorex.transaction.assets.TransferTransaction.MaxAttachmentSize

import scala.concurrent.Await
import scala.concurrent.Future.{sequence, traverse}
import scala.concurrent.duration._

class MassTransferTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val Timeout = 2.minutes
  private val AssetQuantity = 100.waves
  private val TransferAmount = 5.waves
  private val LeasingAmount = 5.waves
  private val LeasingFee = 0.003.waves
  private val TransferFee = 0.002.waves
  private val IssueFee = 5.waves

  test("asset transfer changes asset balances and sender's.waves balance is decreased by fee") {
    val f = for {
      (balance1, eff1) <- notMiner.accountBalances(firstAddress)
      (balance2, eff2) <- notMiner.accountBalances(secondAddress)
      (balance3, eff3) <- notMiner.accountBalances(thirdAddress)

      assetId <- sender.issue(firstAddress, "name", "description", AssetQuantity, 8, reissuable = false, IssueFee).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(assetId)

      transfers = List((secondAddress, TransferAmount), (thirdAddress, TransferAmount))
      transferId <- sender.massTransfer(firstAddress, transfers, TransferFee, Some(assetId)).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(transferId)
      _ <- notMiner.assertBalances(firstAddress, balance1 - TransferFee - IssueFee, eff1 - TransferFee - IssueFee)
      _ <- notMiner.assertBalances(secondAddress, balance2, eff2)
      _ <- notMiner.assertBalances(thirdAddress, balance3, eff3)
      _ <- notMiner.assertAssetBalance(firstAddress, assetId, AssetQuantity - TransferAmount - TransferAmount)
      _ <- notMiner.assertAssetBalance(secondAddress, assetId, TransferAmount)
      _ <- notMiner.assertAssetBalance(thirdAddress, assetId, TransferAmount)
    } yield succeed

    Await.result(f, Timeout)
  }

  test("waves transfer changes waves balances") {
    val f = for {
      (balance1, eff1) <- notMiner.accountBalances(firstAddress)
      (balance2, eff2) <- notMiner.accountBalances(secondAddress)
      (balance3, eff3) <- notMiner.accountBalances(thirdAddress)

      transfers = List((secondAddress, TransferAmount), (thirdAddress, TransferAmount))
      transferId <- sender.massTransfer(firstAddress, transfers, TransferFee).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(transferId)
      _ <- notMiner.assertBalances(firstAddress,
        balance1 - TransferAmount - TransferAmount - TransferFee, eff1 - TransferAmount - TransferAmount - TransferFee)
      _ <- notMiner.assertBalances(secondAddress, balance2 + TransferAmount, eff2 + TransferAmount)
      _ <- notMiner.assertBalances(thirdAddress, balance3 + TransferAmount, eff3 + TransferAmount)
    } yield succeed

    Await.result(f, Timeout)
  }

  test("invalid transfer should not be in UTX or blockchain") {
    import MassTransferTransaction.MaxTransferCount
    val address2 = AddressOrAlias.fromString(secondAddress).right.get
    val valid = MassTransferTransaction.create(
      None, sender.privateKey,
      List((address2, TransferAmount)),
      System.currentTimeMillis,
      TransferFee, Array.emptyByteArray).right.get
    val fromFuture = valid.copy(timestamp = valid.timestamp + 1.day.toMillis)
    val tooManyTransfers = valid.copy(transfers = List.fill(MaxTransferCount + 1)((address2, 1)))
    val negativeTransfer = valid.copy(transfers = List((address2, -1)))
    val negativeFee = valid.copy(fee = 0)
    val longAttachment = valid.copy(attachment = ("ab" * MaxAttachmentSize).getBytes)

    val invalidTransfers = Seq(fromFuture, tooManyTransfers, negativeTransfer, negativeFee, longAttachment)
    for (tx <- invalidTransfers) {
      val id = tx.id()
      val req = createSignedMassTransferRequest(tx)
      val f = for {
        _ <- assertBadRequest(sender.signedMassTransfer(req))
        _ <- sequence(nodes.map(_.ensureTxDoesntExist(id.base58)))
      } yield succeed

      Await.result(f, Timeout)
    }
  }

  test("can not make transfer without having enough waves") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)
      (balance1, eff1) <- notMiner.accountBalances(firstAddress)
      (balance2, eff2) <- notMiner.accountBalances(secondAddress)
      (balance3, eff3) <- notMiner.accountBalances(thirdAddress)

      transfers = List((secondAddress, balance1 / 2), (thirdAddress, balance1 / 2))
      transferFailureAssertion <- assertBadRequest(sender.massTransfer(firstAddress, transfers, TransferFee))

      _ <- traverse(nodes)(_.waitForHeight(fb + 2))
      _ <- notMiner.assertBalances(firstAddress, balance1, eff1)
      _ <- notMiner.assertBalances(secondAddress, balance2, eff2)
      _ <- notMiner.assertBalances(thirdAddress, balance3, eff3)
    } yield transferFailureAssertion

    Await.result(f, Timeout)
  }

  test("can not make transfer without having enough of effective balance") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)
      (balance1, eff1) <- notMiner.accountBalances(firstAddress)
      (balance2, eff2) <- notMiner.accountBalances(secondAddress)

      leaseTxId <- sender.lease(firstAddress, secondAddress, LeasingAmount, LeasingFee).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(leaseTxId)

      transfers = List((secondAddress, balance1 - LeasingFee - TransferFee))
      transferFailureAssertion <- assertBadRequest(sender.massTransfer(firstAddress, transfers, TransferFee))

      _ <- traverse(nodes)(_.waitForHeight(fb + 2))
      _ <- notMiner.assertBalances(firstAddress, balance1 - LeasingFee, eff1 - LeasingAmount - LeasingFee)
      _ <- notMiner.assertBalances(secondAddress, balance2, eff2 + LeasingAmount)
    } yield transferFailureAssertion

    Await.result(f, Timeout)
  }

  private def createSignedMassTransferRequest(tx: MassTransferTransaction): SignedMassTransferRequest = {
    import tx._
    SignedMassTransferRequest(
      Base58.encode(tx.sender.publicKey),
      assetId.map(_.base58),
      transfers.map { case (address, amount) => (address.stringRepr, amount) },
      fee,
      timestamp,
      attachment.headOption.map(_ => Base58.encode(attachment)),
      signature.base58
    )
  }
}
