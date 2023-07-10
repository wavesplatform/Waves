package com.wavesplatform.it.sync.transactions

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme, Alias}
import com.wavesplatform.api.http.ApiError.WrongJson
import com.wavesplatform.api.http.requests.{MassTransferRequest, SignedMassTransferRequest}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.it.api.MassTransferTransactionInfo
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util.TxHelpers
import com.wavesplatform.protobuf.transaction.{MassTransferTransactionData, PBRecipients, Recipient}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.transfer.MassTransferTransaction.{MaxTransferCount, Transfer}
import com.wavesplatform.transaction.transfer.TransferTransaction.MaxAttachmentSize
import com.wavesplatform.transaction.{Proofs, TxVersion}
import play.api.libs.json.*

import scala.concurrent.duration.*
import scala.util.Random

class MassTransferTransactionSuite extends BaseTransactionSuite {

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    // explicitly create an address in node's wallet
    sender.postForm("/addresses")
  }

  private def fakeSignature = ByteStr(Array.fill(64)(Random.nextInt().toByte))

  test("asset mass transfer changes asset balances and sender's.waves balance is decreased by fee.") {
    for (v <- massTransferTxSupportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)

      val transfers = List(Transfer(secondAddress, 1000))
      val assetId   = sender.issue(firstKeyPair, "name", "description", issueAmount, 8, reissuable = false, issueFee).id
      nodes.waitForHeightAriseAndTxPresent(assetId)

      val massTransferTransactionFee = calcMassTransferFee(transfers.size)
      val massTransferTx             = sender.massTransfer(firstKeyPair, transfers, massTransferTransactionFee, assetId = Some(assetId), version = v)
      nodes.waitForHeightAriseAndTxPresent(massTransferTx.id)
      if (v > 1) {
        massTransferTx.chainId shouldBe Some(AddressScheme.current.chainId)
        sender.transactionInfo[MassTransferTransactionInfo](massTransferTx.id).chainId shouldBe Some(AddressScheme.current.chainId)
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
      val transfers        = List(Transfer(secondAddress, 1000), Transfer(thirdAddress, 2 * 1000))

      val massTransferTransactionFee = calcMassTransferFee(transfers.size)
      val transferId                 = sender.massTransfer(firstKeyPair, transfers, massTransferTransactionFee, version = v).id
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
      val transfers        = List(Transfer(secondAddress, balance1 / 2), Transfer(thirdAddress, balance1 / 2))

      assertBadRequestAndResponse(
        sender.massTransfer(firstKeyPair, transfers, calcMassTransferFee(transfers.size), version = v),
        "Attempt to transfer unavailable funds"
      )

      nodes.waitForHeightArise()
      miner.assertBalances(firstAddress, balance1, eff1)
      miner.assertBalances(secondAddress, balance2, eff2)
    }
  }

  test("can not make mass transfer when fee less then required") {
    for (v <- massTransferTxSupportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)
      val transfers        = List(Transfer(secondAddress, transferAmount))

      assertBadRequestAndResponse(
        sender.massTransfer(firstKeyPair, transfers, calcMassTransferFee(transfers.size) - 1, version = v),
        "Fee .* does not exceed minimal value"
      )
      nodes.waitForHeightArise()
      miner.assertBalances(firstAddress, balance1, eff1)
      miner.assertBalances(secondAddress, balance2, eff2)
    }
  }

  test("can not make mass transfer without having enough of effective balance") {
    for (v <- massTransferTxSupportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)
      val transfers        = List(Transfer(secondAddress, balance1 - 2 * minFee))

      val leaseTxId = sender.lease(firstKeyPair, secondAddress, leasingAmount, minFee).id
      nodes.waitForHeightAriseAndTxPresent(leaseTxId)

      assertBadRequestAndResponse(
        sender.massTransfer(firstKeyPair, transfers, calcMassTransferFee(transfers.size), version = v),
        "Attempt to transfer unavailable funds"
      )
      nodes.waitForHeightArise()
      miner.assertBalances(firstAddress, balance1 - minFee, eff1 - leasingAmount - minFee)
      miner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)

      sender.cancelLease(firstKeyPair, leaseTxId, waitForTx = true)
    }
  }

  test("invalid transfer should not be in UTX or blockchain") {
    import com.wavesplatform.transaction.transfer.*

    for (_ <- massTransferTxSupportedVersions) {
      def request(
          transfers: List[Transfer] = List(Transfer(secondAddress, transferAmount)),
          fee: Long = calcMassTransferFee(1),
          timestamp: Long = System.currentTimeMillis,
          attachment: Array[Byte] = Array.emptyByteArray
      ): (SignedMassTransferRequest, Option[ByteStr]) = {
        val txEi = for {
          parsedTransfers <- MassTransferTransaction.parseTransfersList(transfers)
          tx <- MassTransferTransaction.selfSigned(
            1.toByte,
            sender.keyPair,
            Waves,
            parsedTransfers,
            fee,
            timestamp,
            ByteStr(attachment)
          )
        } yield tx

        val (signature, idOpt) = txEi.fold(_ => (Proofs(List(fakeSignature)), None), tx => (tx.proofs, Some(tx.id())))

        val req = SignedMassTransferRequest(
          Some(TxVersion.V1),
          sender.publicKey.toString,
          None,
          transfers,
          fee,
          timestamp,
          ByteStr(attachment),
          signature
        )

        (req, idOpt)
      }

      def negativeTransferAmountRequest: (SignedMassTransferRequest, Option[ByteStr]) = {
        val recipient = secondKeyPair

        val transfers  = List(Transfer(recipient.toAddress.toString, -1))
        val attachment = ByteStr(Array.emptyByteArray)
        val fee        = calcMassTransferFee(1)
        val timestamp  = System.currentTimeMillis()
        val version    = TxVersion.V1
        val mttdTransfers = transfers.map { t =>
          MassTransferTransactionData.Transfer(
            Some(Recipient.of(Recipient.Recipient.PublicKeyHash(PBRecipients.create(Address.fromPublicKey(recipient.publicKey)).getPublicKeyHash))),
            t.amount
          )
        }

        val bodyBytes =
          TxHelpers.massTransferBodyBytes(sender.keyPair, None, mttdTransfers, ByteString.copyFrom(attachment.arr), fee, timestamp, version)

        (
          SignedMassTransferRequest(
            Some(version),
            sender.publicKey.toString,
            None,
            transfers,
            fee,
            timestamp,
            ByteStr(Array.emptyByteArray),
            Proofs(Seq(bodyBytes))
          ),
          Some(ByteStr(crypto.fastHash(bodyBytes.arr)))
        )
      }

      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val invalidTransfers = Seq(
        (request(timestamp = System.currentTimeMillis + 1.day.toMillis), "Transaction timestamp .* is more than .*ms in the future"),
        (
          request(transfers = List.fill(MaxTransferCount + 1)(Transfer(secondAddress, 1)), fee = calcMassTransferFee(MaxTransferCount + 1)),
          s"Number of transfers ${MaxTransferCount + 1} is greater than 100"
        ),
        (negativeTransferAmountRequest, "negative amount: -1 of asset"),
        (request(fee = 0), "insufficient fee"),
        (request(fee = 99999), "Fee .* does not exceed minimal value"),
        (request(attachment = ("a" * (MaxAttachmentSize + 1)).getBytes("UTF-8")), "exceeds maximum length")
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
      val fee              = calcMassTransferFee(MaxTransferCount)
      val amount           = (balance1 - fee) / MaxTransferCount

      val transfers  = List.fill(MaxTransferCount)(Transfer(firstAddress, amount))
      val transferId = sender.massTransfer(firstKeyPair, transfers, fee, version = v).id

      nodes.waitForHeightAriseAndTxPresent(transferId)
      miner.assertBalances(firstAddress, balance1 - fee, eff1 - fee)
    }
  }

  test("transaction requires a proof") {
    for (v <- massTransferTxSupportedVersions) {
      val fee       = calcMassTransferFee(2)
      val transfers = Seq(Transfer(secondAddress, 1000), Transfer(thirdAddress, 1000))
      val signedMassTransfer: JsObject = {
        val rs = sender.postJsonWithApiKey(
          "/transactions/sign",
          Json.obj(
            "type"      -> MassTransferTransaction.typeId,
            "version"   -> v,
            "sender"    -> firstAddress,
            "transfers" -> transfers,
            "fee"       -> fee
          )
        )
        Json.parse(rs.getResponseBody).as[JsObject]
      }

      def id(obj: JsObject) = obj.value("id").as[String]

      val noProof = signedMassTransfer - "proofs"
      assertBadRequestAndResponse(sender.postJson("/transactions/broadcast", noProof), s"${WrongJson.WrongJsonDataMessage}.*proofs.*missing")
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
        val aliasId = sender.createAlias(secondKeyPair, alias, minFee).id
        nodes.waitForHeightAriseAndTxPresent(aliasId)
        minFee
      } else 0

      val aliasFull = sender.aliasByAddress(secondAddress).find(_.endsWith(alias)).get

      val transfers = List(Transfer(firstAddress, 0), Transfer(aliasFull, 1000))

      val massTransferTransactionFee = calcMassTransferFee(transfers.size)
      val transferId                 = sender.massTransfer(firstKeyPair, transfers, massTransferTransactionFee, version = v).id
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
    for (v <- massTransferTxSupportedVersions) {
      val transfers = List(Transfer(firstAddress, 5.waves), Transfer(secondAddress, 2.waves), Transfer(thirdAddress, 3.waves))
      val txId      = sender.massTransfer(firstKeyPair, transfers, 300000, version = v).id
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
      val aliases        = List(s"alias1v$v", s"alias2v$v")
      val createAliasTxs = aliases.map(sender.createAlias(secondKeyPair, _, 100000).id)
      createAliasTxs.foreach(sender.waitForTransaction(_))

      val transfers = aliases.map { alias =>
        Transfer(Alias.create(alias).explicitGet().toString, 2.waves)
      }
      val txId = sender.massTransfer(firstKeyPair, transfers, 300000, version = v).id
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
}
