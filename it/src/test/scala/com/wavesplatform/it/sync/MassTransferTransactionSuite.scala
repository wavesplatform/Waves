package com.wavesplatform.it.transactions

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import scorex.account.AddressOrAlias
import scorex.api.http.assets.SignedMassTransferRequest
import scorex.crypto.encode.Base58
import scorex.transaction.Proofs
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.MassTransferTransaction
import scorex.transaction.assets.MassTransferTransaction.MaxTransferCount
import scorex.transaction.assets.MassTransferTransaction.{ParsedTransfer, Transfer}

import scala.concurrent.duration._
import scorex.transaction.assets.TransferTransaction.MaxAttachmentSize

class MassTransferTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

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

  test("invalid transfer should not be in UTX or blockchain") {
    import scorex.transaction.assets.MassTransferTransaction.MaxTransferCount
    val address2 = AddressOrAlias.fromString(secondAddress).right.get
    val valid = MassTransferTransaction.selfSigned(
      Proofs.Version, None, sender.privateKey,
      List(ParsedTransfer(address2, transferAmount)),
      System.currentTimeMillis,
      calcFee(1), Array.emptyByteArray).right.get
    val fromFuture = valid.copy(timestamp = valid.timestamp + 1.day.toMillis)
    val tooManyTransfers = valid.copy(transfers = List.fill(MaxTransferCount + 1)(ParsedTransfer(address2, 1)), fee = calcFee(MaxTransferCount + 1))
    val negativeAmountTransfer = valid.copy(transfers = List(ParsedTransfer(address2, -1)))
    val negativeFee = valid.copy(fee = -1)
    val longAttachment = valid.copy(attachment = ("ab" * MaxAttachmentSize).getBytes)
    val invalidTransfers = Seq(fromFuture, tooManyTransfers, negativeAmountTransfer, negativeFee, longAttachment)
    for (tx <- invalidTransfers) {
      val id = tx.id()
      val req = createSignedMassTransferRequest(tx)
      assertBadRequest2(sender.signedMassTransfer(req))
      nodes.foreach(_.ensureTxDoesntExist(id.base58))
    }
  }


  private def calcFee(numberOfRecipients: Int): Long = {
    transferFee + numberOfRecipients * massTransferFeePerTransfer
  }


  test("huuuge transactions are allowed") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val fee = calcFee(MaxTransferCount)
    val amount = (balance1 - fee) / MaxTransferCount

    val transfers = List.fill(MaxTransferCount)(Transfer(firstAddress, amount))
    val transferId = sender.massTransfer(firstAddress, transfers, fee).id

    nodes.waitForHeightAraiseAndTxPresent(transferId)
    notMiner.assertBalances(firstAddress, balance1 - fee, eff1 - fee)
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
      proofs.base58().toList
    )
  }

  test("try to make mass transfer if use alias for address") {

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)

    val alias = "masstest_alias"
    
    val aliasFee = if (!sender.aliasByAddress(secondAddress).exists(_.endsWith(alias))){
      val aliasId = sender.createAlias(secondAddress, alias, transferFee).id
      nodes.waitForHeightAraiseAndTxPresent(aliasId)
      transferFee
    } else 0

    val aliasFull = sender.aliasByAddress(secondAddress).find(_.endsWith(alias)).get

    val transfers = List(Transfer(firstAddress, 0), Transfer(aliasFull, transferAmount))

    val massTransferTransactionFee = calcFee(transfers.size)
    val transferId = sender.massTransfer(firstAddress, transfers, massTransferTransactionFee).id
    nodes.waitForHeightAraiseAndTxPresent(transferId)

    notMiner.assertBalances(firstAddress, balance1 - massTransferTransactionFee - transferAmount, eff1 - massTransferTransactionFee - transferAmount)
    notMiner.assertBalances(secondAddress, balance2 + transferAmount - aliasFee, eff2 + transferAmount - aliasFee)
  }
}
