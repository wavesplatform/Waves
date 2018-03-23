package com.wavesplatform.it.sync

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import play.api.libs.json.{JsNumber, JsObject, Json}
import scorex.api.http.assets.SignedMassTransferRequest
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.MassTransferTransaction
import scorex.transaction.assets.MassTransferTransaction.{MaxTransferCount, ParsedTransfer, Transfer}

import scala.concurrent.duration._
import scala.util.Random

class MassTransferTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val assetQuantity              = 100.waves
  private val transferAmount             = 5.waves
  private val leasingAmount              = 5.waves
  private val leasingFee                 = 0.003.waves
  private val transferFee                = notMiner.settings.feesSettings.fees(TransactionType.TransferTransaction.id)(0).fee
  private val issueFee                   = 1.waves
  private val massTransferFeePerTransfer = notMiner.settings.feesSettings.fees(TransactionType.MassTransferTransaction.id)(0).fee

  private def calcFee(numberOfRecipients: Int): Long = {
    transferFee + numberOfRecipients * massTransferFeePerTransfer
  }

  private def fakeSignature = Base58.encode(Array.fill(64)(Random.nextInt.toByte))

  test("asset mass transfer changes asset balances and sender's.waves balance is decreased by fee.") {

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)

    val transfers = List(Transfer(secondAddress, transferAmount))
    val assetId   = sender.issue(firstAddress, "name", "description", assetQuantity, 8, reissuable = false, issueFee).id
    nodes.waitForHeightAriseAndTxPresent(assetId)

    val massTransferTransactionFee = calcFee(transfers.size)
    val transferId                 = sender.massTransfer(firstAddress, transfers, massTransferTransactionFee, Some(assetId)).id
    nodes.waitForHeightAriseAndTxPresent(transferId)

    notMiner.assertBalances(firstAddress, balance1 - massTransferTransactionFee - issueFee, eff1 - massTransferTransactionFee - issueFee)
    notMiner.assertAssetBalance(firstAddress, assetId, assetQuantity - transferAmount)
    notMiner.assertBalances(secondAddress, balance2, eff2)
    notMiner.assertAssetBalance(secondAddress, assetId, transferAmount)
  }

  test("waves mass transfer changes waves balances") {

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
    val (balance3, eff3) = notMiner.accountBalances(thirdAddress)
    val transfers        = List(Transfer(secondAddress, transferAmount), Transfer(thirdAddress, 2 * transferAmount))

    val massTransferTransactionFee = calcFee(transfers.size)
    val transferId                 = sender.massTransfer(firstAddress, transfers, massTransferTransactionFee).id
    nodes.waitForHeightAriseAndTxPresent(transferId)

    notMiner.assertBalances(firstAddress,
                            balance1 - massTransferTransactionFee - 3 * transferAmount,
                            eff1 - massTransferTransactionFee - 3 * transferAmount)
    notMiner.assertBalances(secondAddress, balance2 + transferAmount, eff2 + transferAmount)
    notMiner.assertBalances(thirdAddress, balance3 + 2 * transferAmount, eff3 + 2 * transferAmount)
  }

  test("can not make mass transfer without having enough waves") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
    val transfers        = List(Transfer(secondAddress, balance1 / 2), Transfer(thirdAddress, balance1 / 2))

    assertBadRequestAndResponse(sender.massTransfer(firstAddress, transfers, calcFee(transfers.size)), "negative waves balance")

    nodes.waitForHeightAraise()
    notMiner.assertBalances(firstAddress, balance1, eff1)
    notMiner.assertBalances(secondAddress, balance2, eff2)
  }

  test("can not make mass transfer when fee less then mininal ") {

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
    val transfers        = List(Transfer(secondAddress, transferAmount))

    assertBadRequestAndResponse(sender.massTransfer(firstAddress, transfers, transferFee), "Fee .* does not exceed minimal value")
    nodes.waitForHeightAraise()
    notMiner.assertBalances(firstAddress, balance1, eff1)
    notMiner.assertBalances(secondAddress, balance2, eff2)
  }

  test("can not make mass transfer without having enough of effective balance") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
    val transfers        = List(Transfer(secondAddress, balance1 - leasingFee - transferFee))

    val leaseTxId = sender.lease(firstAddress, secondAddress, leasingAmount, leasingFee).id
    nodes.waitForHeightAriseAndTxPresent(leaseTxId)

    assertBadRequestAndResponse(sender.massTransfer(firstAddress, transfers, calcFee(transfers.size)), "negative waves balance")
    nodes.waitForHeightAraise()
    notMiner.assertBalances(firstAddress, balance1 - leasingFee, eff1 - leasingAmount - leasingFee)
    notMiner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)
  }

  test("invalid transfer should not be in UTX or blockchain") {
    import scorex.transaction.assets.TransferTransaction.MaxAttachmentSize

    def request(version: Byte = MassTransferTransaction.Version,
                transfers: List[Transfer] = List(Transfer(secondAddress, transferAmount)),
                fee: Long = calcFee(1),
                timestamp: Long = System.currentTimeMillis,
                attachment: Array[Byte] = Array.emptyByteArray) = {
      val txEi = for {
        parsedTransfers <- MassTransferTransaction.parseTransfersList(transfers)
        tx <- MassTransferTransaction.selfSigned(version, None, sender.privateKey, parsedTransfers, timestamp, fee, attachment)
      } yield tx

      val (signature, idOpt) = txEi.fold(
        _ => (List(fakeSignature), None),
        tx => (tx.proofs.base58().toList, Some(tx.id())))

      val req = SignedMassTransferRequest(version,
        Base58.encode(sender.publicKey.publicKey),
        None, transfers, fee, timestamp,
        attachment.headOption.map(_ => Base58.encode(attachment)),
        signature)

      (req, idOpt)
    }

    implicit val w =
      Json.writes[SignedMassTransferRequest].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(TransactionType.MassTransferTransaction.id)))

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val invalidTransfers = Seq(
      (request(timestamp = System.currentTimeMillis + 1.day.toMillis),
        "Transaction .* is from far future"),
      (request(transfers = List.fill(MaxTransferCount + 1)(Transfer(secondAddress, 1)), fee = calcFee(MaxTransferCount + 1)),
        "Number of transfers is greater than 100"),
      (request(transfers = List(Transfer(secondAddress, -1))),
        "One of the transfers has negative amount"),
      (request(fee = 0),
        "insufficient fee"),
      (request(fee = 99999),
        "Fee .* does not exceed minimal value"),
      (request(attachment = ("a" * (MaxAttachmentSize + 1)).getBytes),
        "invalid.attachment"))

    for (((req, idOpt), diag) <- invalidTransfers) {
      assertBadRequestAndResponse(sender.broadcastRequest(req), diag)
      idOpt.foreach(id => nodes.foreach(_.ensureTxDoesntExist(id.base58)))
    }

    nodes.waitForHeightAraise()
    notMiner.assertBalances(firstAddress, balance1, eff1)
  }

  test("huuuge transactions are allowed") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val fee              = calcFee(MaxTransferCount)
    val amount           = (balance1 - fee) / MaxTransferCount

    val transfers  = List.fill(MaxTransferCount)(Transfer(firstAddress, amount))
    val transferId = sender.massTransfer(firstAddress, transfers, fee).id

    nodes.waitForHeightAriseAndTxPresent(transferId)
    notMiner.assertBalances(firstAddress, balance1 - fee, eff1 - fee)
  }

  test("transaction requires a proof") {
    val fee       = calcFee(2)
    val transfers = Seq(Transfer(secondAddress, transferAmount), Transfer(thirdAddress, transferAmount))
    val signedMassTransfer: JsObject = {
      val rs = sender.postJsonWithApiKey("/transactions/sign", Json.obj(
        "type" -> TransactionType.MassTransferTransaction.id,
        "version" -> MassTransferTransaction.Version,
        "sender" -> firstAddress,
        "transfers" -> transfers,
        "fee" -> fee))
      Json.parse(rs.getResponseBody).as[JsObject]
    }
    def id(obj: JsObject) = obj.value("id").as[String]

    val noProof = signedMassTransfer - "proofs"
    assertBadRequestAndResponse(sender.postJson("/transactions/broadcast", noProof), "failed to parse json message.*proofs.*missing")
    nodes.foreach(_.ensureTxDoesntExist(id(noProof)))

    val badProof = signedMassTransfer ++ Json.obj("proofs" -> Seq(fakeSignature))
    assertBadRequestAndResponse(sender.postJson("/transactions/broadcast", badProof), "proof doesn't validate as signature")
    nodes.foreach(_.ensureTxDoesntExist(id(badProof)))

    val withProof = signedMassTransfer
    assert((withProof \ "proofs").as[Seq[String]].lengthCompare(1) == 0)
    sender.postJson("/transactions/broadcast", withProof)
    nodes.waitForHeightAriseAndTxPresent(id(withProof))
  }

  test("try to make mass transfer if use alias for address") {

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)

    val alias = "masstest_alias"

    val aliasFee = if (!sender.aliasByAddress(secondAddress).exists(_.endsWith(alias))) {
      val aliasId = sender.createAlias(secondAddress, alias, transferFee).id
      nodes.waitForHeightAriseAndTxPresent(aliasId)
      transferFee
    } else 0

    val aliasFull = sender.aliasByAddress(secondAddress).find(_.endsWith(alias)).get

    val transfers = List(Transfer(firstAddress, 0), Transfer(aliasFull, transferAmount))

    val massTransferTransactionFee = calcFee(transfers.size)
    val transferId                 = sender.massTransfer(firstAddress, transfers, massTransferTransactionFee).id
    nodes.waitForHeightAriseAndTxPresent(transferId)

    notMiner.assertBalances(firstAddress, balance1 - massTransferTransactionFee - transferAmount, eff1 - massTransferTransactionFee - transferAmount)
    notMiner.assertBalances(secondAddress, balance2 + transferAmount - aliasFee, eff2 + transferAmount - aliasFee)
  }
}
