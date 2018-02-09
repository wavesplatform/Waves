package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import scorex.transaction.TransactionParser.TransactionType
import com.wavesplatform.it.api.SyncHttpApi._
import scorex.transaction.assets.MassTransferTransaction.Transfer

import scala.concurrent.duration._

class MassTransferTransactionSuite extends BaseTransactionSuite {

  private val Timeout = 2.minutes
  private val assetQuantity = 100.waves
  private val transferAmount = 5.waves
  private val leasingAmount = 5.waves
  private val leasingFee = 0.003.waves
  private val transferFee = notMiner.settings.feesSettings.fees(TransactionType.TransferTransaction.id)(0).fee
  private val issueFee = 1.waves
  private val massTransferFeePerTransfer = notMiner.settings.feesSettings.fees(TransactionType.MassTransferTransaction.id)(0).fee


  test("asset mass transfer changes asset balances and sender's.waves balance is decreased by fee.") {

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)

    val transfers = List(Transfer(secondAddress, transferAmount))
    val assetId = sender.issue(firstAddress, "name", "description", assetQuantity, 8, reissuable = false, issueFee).id
    nodes.waitForHeightAraiseAndTxPresent(assetId)


    val massTransferTransactionFee = calcFee(transfers.size)
    val transferId = sender.massTransfer(firstAddress, transfers, massTransferTransactionFee, Some(assetId)).id
    nodes.waitForHeightAraiseAndTxPresent(transferId)


    notMiner.assertBalances(firstAddress, balance1 - massTransferTransactionFee - issueFee, eff1 - massTransferTransactionFee - issueFee)
    notMiner.assertAssetBalance(firstAddress, assetId, assetQuantity - transferAmount)
    notMiner.assertBalances(secondAddress, balance2, eff2)
    notMiner.assertAssetBalance(secondAddress, assetId, transferAmount)
  }

  test("waves mass transfer changes waves balances") {

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
    val (balance3, eff3) = notMiner.accountBalances(thirdAddress)
    val transfers = List(Transfer(secondAddress, transferAmount), Transfer(thirdAddress, 2 * transferAmount))

    val massTransferTransactionFee = calcFee(transfers.size)
    val transferId = sender.massTransfer(firstAddress, transfers, massTransferTransactionFee).id
    nodes.waitForHeightAraiseAndTxPresent(transferId)

    notMiner.assertBalances(firstAddress, balance1 - massTransferTransactionFee - 3 * transferAmount, eff1 - massTransferTransactionFee - 3 * transferAmount)
    notMiner.assertBalances(secondAddress, balance2 + transferAmount, eff2 + transferAmount)
    notMiner.assertBalances(thirdAddress, balance3 + 2 * transferAmount, eff3 + 2 * transferAmount)
  }

  test("can not make mass transfer without having enough waves") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
    val transfers = List(Transfer(secondAddress, balance1 / 2), Transfer(thirdAddress, balance1 / 2))

    assertBadRequest2(sender.massTransfer(firstAddress, transfers, calcFee(transfers.size)))

    nodes.waitForHeightAraise()
    notMiner.assertBalances(firstAddress, balance1, eff1)
    notMiner.assertBalances(secondAddress, balance2, eff2)
  }

  test("try to make mass transfer when fee less then mininal ") {

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
    val transfers = List(Transfer(secondAddress, transferAmount))

    assertBadRequest2(sender.massTransfer(firstAddress, transfers, transferFee))
    nodes.waitForHeightAraise()
    notMiner.assertBalances(firstAddress, balance1, eff1)
    notMiner.assertBalances(secondAddress, balance2, eff2)
  }

  test("can not make mass transfer without having enough of effective balance") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
    val transfers = List(Transfer(secondAddress, balance1 - leasingFee - transferFee))

    val leaseTxId = sender.lease(firstAddress, secondAddress, leasingAmount, leasingFee).id
    nodes.waitForHeightAraiseAndTxPresent(leaseTxId)

    assertBadRequest2(sender.massTransfer(firstAddress, transfers, transferFee))
    nodes.waitForHeightAraise()
    notMiner.assertBalances(firstAddress, balance1 - leasingFee, eff1 - leasingAmount - leasingFee)
    notMiner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)
  }

  private def calcFee(numberOfRecipients: Int): Long = {
    transferFee + numberOfRecipients * massTransferFeePerTransfer
  }

  //
  //  private def createSignedMassTransferRequest(tx: MassTransferTransaction): SignedMassTransferRequest = {
  //    import tx._
  //    SignedMassTransferRequest(
  //      Base58.encode(tx.sender.publicKey),
  //      assetId.map(_.base58),
  //      transfers.map { case ParsedTransfer(address, amount) => Transfer(address.stringRepr, amount) },
  //      fee,
  //      timestamp,
  //      attachment.headOption.map(_ => Base58.encode(attachment)),
  //      signature.base58
  //    )
  //  }
}
