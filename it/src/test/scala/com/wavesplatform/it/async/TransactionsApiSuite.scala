package com.wavesplatform.it

import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.state2._
import org.asynchttpclient.util.HttpConstants
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
import scorex.api.http.assets.MassTransferRequest
import scorex.crypto.encode.Base58
import scorex.transaction.assets.MassTransferTransaction.Transfer

import scala.concurrent.Await
import scala.concurrent.duration._

class TransactionsApiSuite extends BaseTransactionSuite {

  private val timeout = 2.minutes

  test("height should always be reported for transactions") {
    val f = for {
      txId <- sender.transfer(firstAddress, secondAddress, 1.waves, fee = 1.waves).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(txId)

      jsv1 <- sender.get(s"/transactions/info/$txId").as[JsValue]
      hasPositiveHeight1 = (jsv1 \ "height").asOpt[Int].map(_ > 0)
      _ = assert(hasPositiveHeight1.getOrElse(false))

      jsv2 <- sender.get(s"/transactions/address/$firstAddress/limit/1").as[JsArray]
      hasPositiveHeight2 = (jsv2(0)(0) \ "height").asOpt[Int].map(_ > 0)
      _ = assert(hasPositiveHeight2.getOrElse(false))
    } yield succeed

    Await.result(f, timeout)
  }

  test("/transactions/sign should handle erroneous input") {
    def assertSignBadJson(json: JsObject) =
      assertBadRequestAndMessage(sender.postJsonWithApiKey("/transactions/sign", json), "failed to parse json message")

    val json = Json.obj(
      "type" -> 10,
      "sender" -> firstAddress,
      "alias" -> "alias",
      "fee" -> 100000)
    val f = for {
      _ <- assertSignBadJson(json - "type")
      _ <- assertSignBadJson(json + ("type" -> Json.toJson(-100)))
      _ <- assertSignBadJson(json - "alias")
    } yield succeed

    Await.result(f, timeout)
  }

  test("/transactions/sign should respect timestamp if specified") {
    val timestamp = 1500000000000L
    val json = Json.obj(
      "type" -> 10,
      "sender" -> firstAddress,
      "alias" -> "alias",
      "fee" -> 100000,
      "timestamp" -> timestamp)
    val f = for {
      r <- sender.postJsonWithApiKey("/transactions/sign", json)
      _ = assert(r.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
      _ = assert((Json.parse(r.getResponseBody) \ "timestamp").as[Long] == timestamp)
    } yield succeed

    Await.result(f, timeout)
  }

  test("/transactions/broadcast should handle erroneous input") {
    def assertBroadcastBadJson(json: JsObject, expectedMessage: String) =
      assertBadRequestAndMessage(sender.postJson("/transactions/broadcast", json), expectedMessage)

    val timestamp = System.currentTimeMillis
    val json = Json.obj(
      "type" -> 10,
      "senderPublicKey" -> "8LbAU5BSrGkpk5wbjLMNjrbc9VzN9KBBYv9X8wGpmAJT",
      "alias" -> "alias",
      "fee" -> 100000,
      "timestamp" -> timestamp,
      "signature" -> "A" * 64)
    val f = for {
      _ <- assertBroadcastBadJson(json - "type", "failed to parse json message")
      _ <- assertBroadcastBadJson(json - "type" + ("type" -> Json.toJson(88)), "failed to parse json message")
      _ <- assertBroadcastBadJson(json - "alias", "failed to parse json message")
      _ <- assertBroadcastBadJson(json, "invalid signature")
    } yield succeed

    Await.result(f, timeout)
  }

  test("/transactions/sign should produce issue/reissue/burn/transfer transactions that are good for /transactions/broadcast") {
    val issueId = signAndBroadcast(Json.obj(
      "type" -> 3,
      "name" -> "Gigacoin",
      "quantity" -> 100.waves,
      "description" -> "Gigacoin",
      "sender" -> firstAddress,
      "decimals" -> 8,
      "reissuable" -> true,
      "fee" -> 1.waves))

    signAndBroadcast(Json.obj(
      "type" -> 5,
      "quantity" -> 200.waves,
      "assetId" -> issueId,
      "sender" -> firstAddress,
      "reissuable" -> false,
      "fee" -> 1.waves))

    signAndBroadcast(Json.obj(
      "type" -> 6,
      "quantity" -> 100.waves,
      "assetId" -> issueId,
      "sender" -> firstAddress,
      "fee" -> 1.waves))

    signAndBroadcast(Json.obj(
      "type" -> 4,
      "sender" -> firstAddress,
      "recipient" -> secondAddress,
      "fee" -> 100000,
      "assetId" -> issueId,
      "amount" -> 1.waves,
      "attachment" -> Base58.encode("asset transfer".getBytes)))
  }

  test("/transactions/sign should produce transfer transaction that is good for /transactions/broadcast") {
    signAndBroadcast(Json.obj(
      "type" -> 4,
      "sender" -> firstAddress,
      "recipient" -> secondAddress,
      "fee" -> 100000,
      "amount" -> 1.waves,
      "attachment" -> Base58.encode("falafel".getBytes)))
  }

  test("/transactions/sign should produce mass transfer transaction that is good for /transactions/broadcast") {
    signAndBroadcast(Json.obj(
      "type" -> 11,
      "version" -> 1,
      "sender" -> firstAddress,
      "transfers" -> Json.toJson(Seq(Transfer(secondAddress, 1.waves), Transfer(thirdAddress, 2.waves))),
      "fee" -> 200000,
      "attachment" -> Base58.encode("masspay".getBytes)),
      usesProofs = true)
  }

  test("/transactions/sign should produce lease/cancel transactions that are good for /transactions/broadcast") {
    val leaseId = signAndBroadcast(Json.obj(
      "type" -> 8,
      "sender" -> firstAddress,
      "amount" -> 1.waves,
      "recipient" -> secondAddress,
      "fee" -> 100000))

    signAndBroadcast(Json.obj(
      "type" -> 9,
      "sender" -> firstAddress,
      "txId" -> leaseId,
      "fee" -> 100000))
  }

  test("/transactions/sign should produce alias transaction that is good for /transactions/broadcast") {
    signAndBroadcast(Json.obj(
      "type" -> 10,
      "sender" -> firstAddress,
      "alias" -> "myalias",
      "fee" -> 100000))
  }

  test("/transactions/sign should produce data transaction that is good for /transactions/broadcast") {
    signAndBroadcast(Json.obj(
      "type" -> 12,
      "version" -> 1,
      "sender" -> firstAddress,
      "data" -> List(
        LongDataEntry("int", 923275292849183L),
        BooleanDataEntry("bool", true),
        BinaryDataEntry("blob", ByteStr(Array.tabulate(445)(_.toByte)))),
      "fee" -> 100000),
      usesProofs = true)
  }

  private def signAndBroadcast(json: JsObject, usesProofs: Boolean = false): String = {
    val f = for {
      rs <- sender.postJsonWithApiKey("/transactions/sign", json)
      _ = assert(rs.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
      body = Json.parse(rs.getResponseBody)
      signed: Boolean = if (usesProofs) {
        val proofs = (body \ "proofs").as[Seq[String]]
        proofs.lengthCompare(1) == 0 && proofs.head.nonEmpty
      } else (body \ "signature").as[String].nonEmpty
      _ = assert(signed)
      rb <- sender.postJson("/transactions/broadcast", body)
      _ = assert(rb.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
      id = (Json.parse(rb.getResponseBody) \ "id").as[String]
      _ = assert(id.nonEmpty)
      _ <- nodes.waitForHeightAraiseAndTxPresent(id)
    } yield id

    Await.result(f, timeout)
  }

  test("reporting MassTransfer transactions") {
    implicit val mtFormat = Json.format[MassTransferRequest]

    val transfers = List(Transfer(firstAddress, 5.waves), Transfer(secondAddress, 2.waves), Transfer(thirdAddress, 3.waves))
    val f = for {
      txId <- sender.massTransfer(firstAddress, transfers, 250000).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(txId)

      // /transactions/txInfo should return complete list of transfers
      txInfo <- sender.get(s"/transactions/info/$txId").as[MassTransferRequest]
      _ = assert(txInfo.transfers.size == 3)

      // /transactions/address should return complete transfers list for the sender...
      txSender <- sender.get(s"/transactions/address/$firstAddress/limit/1").as[JsArray].map(_.apply(0)(0))
      _ = assert(txSender.as[MassTransferRequest].transfers.size == 3)
      _ = assert((txSender \ "transferCount").as[Int] == 3)
      _ = assert((txSender \ "totalAmount").as[Long] == 10.waves)
      transfersAfterTrans = txSender.as[MassTransferRequest].transfers

      _ = assert((transfers.equals(transfersAfterTrans)))

      // ...and compact list for recipients
      txRecipient <- sender.get(s"/transactions/address/$secondAddress/limit/1").as[JsArray].map(_.apply(0)(0))
      _ = assert(txRecipient.as[MassTransferRequest].transfers.size == 1)
      _ = assert((txRecipient \ "transferCount").as[Int] == 3)
      _ = assert((txRecipient \ "totalAmount").as[Long] == 10.waves)
      transferToSecond = txRecipient.as[MassTransferRequest].transfers.head

      _ = assert(transfers contains transferToSecond)

    } yield succeed

    Await.result(f, timeout)
  }
}
