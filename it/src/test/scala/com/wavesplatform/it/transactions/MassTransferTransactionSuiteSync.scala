package com.wavesplatform.it.transactions

import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import scorex.account.AddressOrAlias
import scorex.api.http.assets.SignedMassTransferRequest
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.MassTransferTransaction
import scorex.transaction.assets.MassTransferTransaction.{ParsedTransfer, Transfer}
import scorex.transaction.assets.TransferTransaction.MaxAttachmentSize

import scala.concurrent.Await
import scala.concurrent.Future.{sequence, traverse}
import scala.concurrent.duration._

class MassTransferTransactionSuiteSync extends BaseTransactionSuite with CancelAfterFailure {

  private val Timeout = 2.minutes
  private val AssetQuantity = 100.waves
  private val TransferAmount = 5.waves
  private val LeasingAmount = 5.waves
  private val LeasingFee = 0.003.waves
  private val TransferFee = 0.002.waves
  private val IssueFee = 5.waves
  private val MassTransferFee = notMiner.settings.feesSettings.fees(TransactionType.MassTransferTransaction.id)(0).fee

  test("asset transfer changes asset balances and sender's.waves balance is decreased by fee") {


    log.info(s"feeee - $kghm")
    val f = for {

      (balance1, eff1) <- notMiner.accountBalances(firstAddress)
      (balance2, eff2) <- notMiner.accountBalances(secondAddress)
      (balance3, eff3) <- notMiner.accountBalances(thirdAddress)

      assetId <- sender.issue(firstAddress, "name", "description", AssetQuantity, 8, reissuable = false, IssueFee).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(assetId)

      transfers = List(Transfer(secondAddress, TransferAmount), Transfer(thirdAddress, TransferAmount))
      transferId <- sender.massTransfer(firstAddress, transfers, TransferFee, Some(assetId)).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(transferId)
      _ <- notMiner.assertBalances(firstAddress, balance1 - TransferFee - IssueFee, eff1 - kghm(0).fee - IssueFee)
      _ <- notMiner.assertBalances(secondAddress, balance2, eff2)
      _ <- notMiner.assertBalances(thirdAddress, balance3, eff3)
      _ <- notMiner.assertAssetBalance(firstAddress, assetId, AssetQuantity - TransferAmount - TransferAmount)
      _ <- notMiner.assertAssetBalance(secondAddress, assetId, TransferAmount)
      _ <- notMiner.assertAssetBalance(thirdAddress, assetId, TransferAmount)
    } yield succeed

    Await.result(f, Timeout)
  }


  private def createSignedMassTransferRequest(tx: MassTransferTransaction): SignedMassTransferRequest = {
    import tx._
    SignedMassTransferRequest(
      Base58.encode(tx.sender.publicKey),
      assetId.map(_.base58),
      transfers.map { case ParsedTransfer(address, amount) => Transfer(address.stringRepr, amount) },
      fee,
      timestamp,
      attachment.headOption.map(_ => Base58.encode(attachment)),
      signature.base58
    )
  }
}
