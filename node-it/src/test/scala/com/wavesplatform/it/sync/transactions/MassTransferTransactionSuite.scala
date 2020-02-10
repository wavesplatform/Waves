package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.{AddressScheme, Alias}
import com.wavesplatform.api.http.requests.{MassTransferRequest, SignedMassTransferRequest}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.MassTransferTransaction.{MaxTransferCount, Transfer}
import com.wavesplatform.transaction.transfer.TransferTransaction.MaxAttachmentSize
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{Proofs, TxVersion}
import org.scalatest.CancelAfterFailure
import play.api.libs.json._

import scala.concurrent.duration._
import scala.util.Random

class MassTransferTransactionSuite extends BaseTransactionSuite /*with CancelAfterFailure*/ {

  private def fakeSignature = ByteStr(Array.fill(64)(Random.nextInt.toByte))

  test("asset mass transfer changes asset balances and sender's.waves balance is decreased by fee.") {
    for (v <- massTransferTxSupportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)

      val transfers = List(Transfer(secondAddress, 1000))
      val assetId = sender.issue(firstAddress, "name", "description", issueAmount, 8, reissuable = false, issueFee).id
      nodes.waitForHeightAriseAndTxPresent(assetId)

      val massTransferTransactionFee = calcMassTransferFee(transfers.size)
      val massTransferTx = sender.massTransfer(firstAddress, transfers, massTransferTransactionFee, assetId = Some(assetId), version = v)
      nodes.waitForHeightAriseAndTxPresent(massTransferTx.id)
      if (v > 1) {
        massTransferTx.chainId shouldBe Some(AddressScheme.current.chainId)
        sender.transactionInfo(massTransferTx.id).chainId shouldBe Some(AddressScheme.current.chainId)
      }

      miner.assertBalances(firstAddress, balance1 - massTransferTransactionFee - issueFee, eff1 - massTransferTransactionFee - issueFee)
      miner.assertAssetBalance(firstAddress, assetId, issueAmount - 1000)
      miner.assertBalances(secondAddress, balance2, eff2)
      miner.assertAssetBalance(secondAddress, assetId, 1000)
    }
  }

  test("waves mass transfer changes waves balances") {
    for (v <- massTransferTxSupportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)
      val (balance3, eff3) = miner.accountBalances(thirdAddress)
      val transfers = List(Transfer(secondAddress, 1000), Transfer(thirdAddress, 2 * 1000))

      val massTransferTransactionFee = calcMassTransferFee(transfers.size)
      val transferId = sender.massTransfer(firstAddress, transfers, massTransferTransactionFee, version = v).id
      nodes.waitForHeightAriseAndTxPresent(transferId)

      miner.assertBalances(
        firstAddress,
        balance1 - massTransferTransactionFee - 3 * 1000,
        eff1 - massTransferTransactionFee - 3 * 1000
      )
      miner.assertBalances(secondAddress, balance2 + 1000, eff2 + 1000)
      miner.assertBalances(thirdAddress, balance3 + 2 * 1000, eff3 + 2 * 1000)
    }
  }

  test("can not make mass transfer without having enough waves") {
    for (v <- massTransferTxSupportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)
      val transfers = List(Transfer(secondAddress, balance1 / 2), Transfer(thirdAddress, balance1 / 2))

      assertBadRequestAndResponse(sender.massTransfer(firstAddress, transfers, calcMassTransferFee(transfers.size), version = v), "Attempt to transfer unavailable funds")

      nodes.waitForHeightArise()
      miner.assertBalances(firstAddress, balance1, eff1)
      miner.assertBalances(secondAddress, balance2, eff2)
    }
  }

  test("can not make mass transfer when fee less then required") {
    for (v <- massTransferTxSupportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)
      val transfers = List(Transfer(secondAddress, transferAmount))

      assertBadRequestAndResponse(sender.massTransfer(firstAddress, transfers, calcMassTransferFee(transfers.size) - 1, version = v), "Fee .* does not exceed minimal value")
      nodes.waitForHeightArise()
      miner.assertBalances(firstAddress, balance1, eff1)
      miner.assertBalances(secondAddress, balance2, eff2)
    }
  }

  test("can not make mass transfer without having enough of effective balance") {
    for (v <- massTransferTxSupportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)
      val transfers = List(Transfer(secondAddress, balance1 - 2 * minFee))

      val leaseTxId = sender.lease(firstAddress, secondAddress, leasingAmount, minFee).id
      nodes.waitForHeightAriseAndTxPresent(leaseTxId)

      assertBadRequestAndResponse(sender.massTransfer(firstAddress, transfers, calcMassTransferFee(transfers.size), version = v), "Attempt to transfer unavailable funds")
      nodes.waitForHeightArise()
      miner.assertBalances(firstAddress, balance1 - minFee, eff1 - leasingAmount - minFee)
      miner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)

      sender.cancelLease(firstAddress, leaseTxId, waitForTx = true)
    }
  }

  test("invalid transfer should not be in UTX or blockchain") {
    import com.wavesplatform.transaction.transfer._

    for (v <- massTransferTxSupportedVersions) {
      def request(
                   transfers: List[Transfer] = List(Transfer(secondAddress, transferAmount)),
                   fee: Long = calcMassTransferFee(1),
                   timestamp: Long = System.currentTimeMillis,
                   attachment: Array[Byte] = Array.emptyByteArray
                 ) = {
        val txEi = for {
          parsedTransfers <- MassTransferTransaction.parseTransfersList(transfers)
          tx <- MassTransferTransaction.selfSigned(
            1.toByte,
            sender.privateKey,
            Waves,
            parsedTransfers,
            fee,
            timestamp,
            Some(Attachment.Bin(attachment))
          )
        } yield tx

        val (signature, idOpt) = txEi.fold(_ => (Proofs(List(fakeSignature)), None), tx => (tx.proofs, Some(tx.id())))

        val req = SignedMassTransferRequest(
          Some(TxVersion.V1),
          Base58.encode(sender.publicKey),
          None,
          transfers,
          fee,
          timestamp,
          Some(Attachment.Bin(attachment)),
          signature
        )

        (req, idOpt)
      }

      import com.wavesplatform.api.http.requests.proofsWrites

      implicit val w =
        Json.writes[SignedMassTransferRequest].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt)))

      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val invalidTransfers = Seq(
        (request(timestamp = System.currentTimeMillis + 1.day.toMillis), "Transaction timestamp .* is more than .*ms in the future"),
        (
          request(transfers = List.fill(MaxTransferCount + 1)(Transfer(secondAddress, 1)), fee = calcMassTransferFee(MaxTransferCount + 1)),
          s"Number of transfers ${MaxTransferCount + 1} is greater than 100"
        ),
        (request(transfers = List(Transfer(secondAddress, -1))), "One of the transfers has negative amount"),
        (request(fee = 0), "insufficient fee"),
        (request(fee = 99999), "Fee .* does not exceed minimal value"),
        (request(attachment = ("a" * (MaxAttachmentSize + 1)).getBytes("UTF-8")), "Too big sequences requested")
      )

      for (((req, idOpt), diag) <- invalidTransfers) {
        assertBadRequestAndResponse(sender.broadcastRequest(req), diag)
        idOpt.foreach(id => nodes.foreach(_.ensureTxDoesntExist(id.toString)))
      }

      nodes.waitForHeightArise()
      miner.assertBalances(firstAddress, balance1, eff1)
    }
  }

  test("huuuge transactions are allowed") {
    for (v <- massTransferTxSupportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val fee = calcMassTransferFee(MaxTransferCount)
      val amount = (balance1 - fee) / MaxTransferCount

      val transfers = List.fill(MaxTransferCount)(Transfer(firstAddress, amount))
      val transferId = sender.massTransfer(firstAddress, transfers, fee, version = v).id

      nodes.waitForHeightAriseAndTxPresent(transferId)
      miner.assertBalances(firstAddress, balance1 - fee, eff1 - fee)
    }
  }

  test("transaction requires a proof") {
    for (v <- massTransferTxSupportedVersions) {
      val fee = calcMassTransferFee(2)
      val transfers = Seq(Transfer(secondAddress, 1000), Transfer(thirdAddress, 1000))
      val signedMassTransfer: JsObject = {
        val rs = sender.postJsonWithApiKey(
          "/transactions/sign",
          Json.obj(
            "type" -> MassTransferTransaction.typeId,
            "version" -> v,
            "sender" -> firstAddress,
            "transfers" -> transfers,
            "fee" -> fee
          )
        )
        Json.parse(rs.getResponseBody).as[JsObject]
      }

      def id(obj: JsObject) = obj.value("id").as[String]

      val noProof = signedMassTransfer - "proofs"
      assertBadRequestAndResponse(sender.postJson("/transactions/broadcast", noProof), "failed to parse json message.*proofs.*missing")
      nodes.foreach(_.ensureTxDoesntExist(id(noProof)))

      val badProof = signedMassTransfer ++ Json.obj("proofs" -> Seq(fakeSignature.toString))
      assertBadRequestAndResponse(sender.postJson("/transactions/broadcast", badProof), "Proof doesn't validate as signature")
      nodes.foreach(_.ensureTxDoesntExist(id(badProof)))

      val withProof = signedMassTransfer
      assert((withProof \ "proofs").as[Seq[String]].lengthCompare(1) == 0)
      sender.postJson("/transactions/broadcast", withProof)
      nodes.waitForHeightAriseAndTxPresent(id(withProof))
    }
  }

  test("try to make mass transfer if use alias for address") {
    for (v <- massTransferTxSupportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)

      val alias = s"masstest_alias$v"

      val aliasFee = if (!sender.aliasByAddress(secondAddress).exists(_.endsWith(alias))) {
        val aliasId = sender.createAlias(secondAddress, alias, minFee).id
        nodes.waitForHeightAriseAndTxPresent(aliasId)
        minFee
      } else 0

      val aliasFull = sender.aliasByAddress(secondAddress).find(_.endsWith(alias)).get

      val transfers = List(Transfer(firstAddress, 0), Transfer(aliasFull, 1000))

      val massTransferTransactionFee = calcMassTransferFee(transfers.size)
      val transferId = sender.massTransfer(firstAddress, transfers, massTransferTransactionFee, version = v).id
      nodes.waitForHeightAriseAndTxPresent(transferId)

      miner.assertBalances(firstAddress, balance1 - massTransferTransactionFee - 1000, eff1 - massTransferTransactionFee - 1000)
      miner.assertBalances(secondAddress, balance2 + 1000 - aliasFee, eff2 + 1000 - aliasFee)
    }
  }

  private def extractTransactionByType(json: JsValue, t: Int): Seq[JsValue] = {
    json.validate[Seq[JsObject]].getOrElse(Seq.empty[JsValue]).filter(_("type").as[Int] == t)
  }

  private def extractTransactionById(json: JsValue, id: String): Option[JsValue] = {
    json.validate[Seq[JsObject]].getOrElse(Seq.empty[JsValue]).find(_("id").as[String] == id)
  }

  test("reporting MassTransfer transactions") {
    implicit val mtFormat: Format[MassTransferRequest] = Json.format[MassTransferRequest]

    for (v <- massTransferTxSupportedVersions) {
      val transfers = List(Transfer(firstAddress, 5.waves), Transfer(secondAddress, 2.waves), Transfer(thirdAddress, 3.waves))
      val txId = sender.massTransfer(firstAddress, transfers, 300000, version = v).id
      nodes.waitForHeightAriseAndTxPresent(txId)

      // /transactions/info/txID should return complete list of transfers
      val txInfo = Json.parse(sender.get(s"/transactions/info/$txId").getResponseBody).as[MassTransferRequest]
      assert(txInfo.transfers.size == 3)

      // /transactions/address should return complete transfers list for the sender...
      val txSender = Json
        .parse(sender.get(s"/transactions/address/$firstAddress/limit/10").getResponseBody)
        .as[JsArray]
        .value
        .map(js => extractTransactionByType(js, 11).head)
        .head
      assert(txSender.as[MassTransferRequest].transfers.size == 3)
      assert((txSender \ "transferCount").as[Int] == 3)
      assert((txSender \ "totalAmount").as[Long] == 10.waves)
      val transfersAfterTrans = txSender.as[MassTransferRequest].transfers
      assert(transfers.equals(transfersAfterTrans))

      // ...and compact list for recipients
      val txRecipient = Json
        .parse(
          sender
            .get(s"/transactions/address/$secondAddress/limit/10")
            .getResponseBody
        )
        .as[JsArray]
        .value
        .map(js => extractTransactionByType(js, 11).head)
        .head

      assert(txRecipient.as[MassTransferRequest].transfers.size == 1)
      assert((txRecipient \ "transferCount").as[Int] == 3)
      assert((txRecipient \ "totalAmount").as[Long] == 10.waves)
      val transferToSecond = txRecipient.as[MassTransferRequest].transfers.head
      assert(transfers contains transferToSecond)
    }
  }

  test("reporting MassTransfer transactions to aliases") {
    for (v <- massTransferTxSupportedVersions) {
      val aliases = List(s"alias1v$v", s"alias2v$v")
      val createAliasTxs = aliases.map(sender.createAlias(secondAddress, _, 100000).id)
      createAliasTxs.foreach(sender.waitForTransaction(_))

      val transfers = aliases.map { alias =>
        Transfer(Alias.create(alias).explicitGet().stringRepr, 2.waves)
      }
      val txId = sender.massTransfer(firstAddress, transfers, 300000, version = v).id
      nodes.waitForHeightAriseAndTxPresent(txId)

      val rawTxs = sender
        .get(s"/transactions/address/$secondAddress/limit/10")
        .getResponseBody

      val recipientTx =
        extractTransactionById(Json.parse(rawTxs).as[JsArray].head.getOrElse(fail("The returned array is empty")), txId)
          .getOrElse(fail(s"Can't find a mass transfer transaction $txId"))

      assert((recipientTx \ "transfers").as[Seq[Transfer]].size == 2)
    }
  }

  test("able to pass typed attachment to transfer transaction V2") {
    val transfers = List(Transfer(firstAddress, transferAmount))
    val txWithStringAtt =
      miner.massTransfer(
        firstAddress,
        transfers,
        calcMassTransferFee(1),
        version = TxVersion.V2,
        typedAttachment = Some(Attachment.Str("qwe")),
        waitForTx = true
      )
    val txWithStringAttInfo = sender.transactionInfo(txWithStringAtt.id)
    txWithStringAttInfo.typedAttachment shouldBe Some(Attachment.Str("qwe"))
    txWithStringAtt.typedAttachment shouldBe Some(Attachment.Str("qwe"))


    val txWithBoolAtt =
      miner.massTransfer(
        firstAddress,
        transfers,
        calcMassTransferFee(1),
        version = TxVersion.V2,
        typedAttachment = Some(Attachment.Bool(true)),
        waitForTx = true
      )
    val txWithBoolAttInfo = sender.transactionInfo(txWithBoolAtt.id)
    txWithBoolAttInfo.typedAttachment shouldBe Some(Attachment.Bool(true))

    val txWithIntAtt =
      miner.massTransfer(
        firstAddress,
        transfers,
        calcMassTransferFee(1),
        version = TxVersion.V2,
        typedAttachment = Some(Attachment.Num(123)),
        waitForTx = true
      )
    val txWithIntAttInfo = sender.transactionInfo(txWithIntAtt.id)
    txWithIntAttInfo.typedAttachment shouldBe Some(Attachment.Num(123))

    val txWithBinaryAtt =
      miner.massTransfer(
        firstAddress,
        transfers,
        calcMassTransferFee(1),
        version = TxVersion.V2,
        typedAttachment = Some(Attachment.Bin(Array[Byte](127.toByte, 0, 1, 1))),
        waitForTx = true
      )
    val txWithBinaryAttInfo = sender.transactionInfo(txWithBinaryAtt.id)
    txWithBinaryAttInfo.typedAttachment shouldBe Some(Attachment.Bin(Array[Byte](127.toByte, 0, 1, 1)))
  }
  test("not able to pass typed attachment to mass transfer transaction V1") {
      assertApiError(
        sender.signAndBroadcast(
          Json.obj(
            "type" -> MassTransferTransaction.typeId,
            "assetId" -> JsNull,
            "sender" -> firstAddress,
            "fee" -> calcMassTransferFee(1),
            "version" -> 1,
            "transfers" -> Json.toJson(List(Transfer(firstAddress, 1000))),
            "attachment" -> Json.toJson(Attachment.Str("somestring"))
          )
        )
      ) { error =>
        error.id shouldBe 10
        error.message shouldBe "Too big sequences requested"
      }
    }

  test("not able to pass not typed attachment to mass transfer transaction V2") {
    assertApiError(
      sender.signAndBroadcast(
        Json.obj(
          "type" -> MassTransferTransaction.typeId,
          "assetId" -> JsNull,
          "sender" -> firstAddress,
          "fee" -> calcMassTransferFee(1),
          "version" -> 2,
          "transfers" -> Json.toJson(List(Transfer(firstAddress, 1000))),
          "attachment" -> "somestring"
        )
      )
    ) { error =>
      error.id shouldBe 199
      error.message shouldBe "Too big sequences requested"
    }
  }
}
