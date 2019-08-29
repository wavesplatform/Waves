package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.http.assets.SignedTransferV1Request
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{someAssetAmount, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.AssetPair.extractAssetId
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, _}
import com.wavesplatform.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, SponsorFeeTransaction}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import org.asynchttpclient.util.HttpConstants
import org.scalatest
import play.api.libs.json._

import scala.util.Random

class SignAndBroadcastApiSuite extends BaseTransactionSuite with NTPTime {
  test("height should always be reported for transactions") {
    val txId = sender.transfer(firstAddress, secondAddress, 1.waves, fee = minFee).id

    sender.waitForTransaction(txId)
    val jsv1               = Json.parse(sender.get(s"/transactions/info/$txId").getResponseBody)
    val hasPositiveHeight1 = (jsv1 \ "height").asOpt[Int].map(_ > 0)
    assert(hasPositiveHeight1.getOrElse(false))

    val response           = sender.get(s"/transactions/address/$firstAddress/limit/1")
    val jsv2               = Json.parse(response.getResponseBody).as[JsArray]
    val hasPositiveHeight2 = (jsv2(0)(0) \ "height").asOpt[Int].map(_ > 0)

    assert(hasPositiveHeight2.getOrElse(false))
  }

  test("/transactions/sign should handle erroneous input") {
    def assertSignBadJson(json: JsObject, expectedMessage: String, code: Int = 400): scalatest.Assertion =
      assertBadRequestAndMessage(sender.postJsonWithApiKey("/transactions/sign", json), expectedMessage, code)

    for (v <- supportedVersions) {
      val json = Json.obj("type" -> CreateAliasTransaction.typeId, "sender" -> firstAddress, "alias" -> "alias", "fee" -> 100000)
      val js   = if (Option(v).isDefined) json ++ Json.obj("version" -> v) else json
      assertSignBadJson(js - "type", "failed to parse json message")
      assertSignBadJson(js + ("type" -> Json.toJson(-100)), "Bad transaction type")
      assertSignBadJson(js - "alias", "failed to parse json message")
    }

    val obsoleteTx =
      Json.obj("type" -> GenesisTransaction.typeId, "sender" -> firstAddress, "recipient" -> firstAddress, "amount" -> 1, "fee" -> 100000)
    assertSignBadJson(obsoleteTx, "transaction type not supported", 501)
    assertSignBadJson(obsoleteTx + ("type" -> Json.toJson(PaymentTransaction.typeId)), "transaction type not supported", 501)

    val bigBaseTx =
      Json.obj("type"       -> TransferTransaction.typeId,
               "sender"     -> firstAddress,
               "recipient"  -> firstAddress,
               "amount"     -> 1,
               "fee"        -> 100000,
               "attachment" -> "W" * 524291)
    assertSignBadJson(bigBaseTx, "base58Decode input exceeds")
  }

  test("/transaction/calculateFee should handle coding size limit") {
    {
      val json =
        Json.obj(
          "type"            -> TransferTransaction.typeId,
          "senderPublicKey" -> sender.publicKey.toString,
          "recipient"       -> secondAddress,
          "fee"             -> 100000,
          "amount"          -> 1,
          "assetId"         -> "W" * 524291
        )
      assertBadRequestAndMessage(sender.calculateFee(json).feeAmount, "base58Decode input exceeds")
    }
  }

  test("/transactions/sign should respect timestamp if specified") {
    val timestamp = 1500000000000L
    for (v <- supportedVersions) {
      val json =
        Json.obj("type" -> CreateAliasTransaction.typeId, "sender" -> firstAddress, "alias" -> "alias", "fee" -> 100000, "timestamp" -> timestamp)
      val js = if (Option(v).isDefined) json ++ Json.obj("version" -> v) else json
      val r  = sender.postJsonWithApiKey("/transactions/sign", js)
      assert(r.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
      assert((Json.parse(r.getResponseBody) \ "timestamp").as[Long] == timestamp)
    }
  }

  test("/transactions/broadcast should handle erroneous input") {
    def assertBroadcastBadJson(json: JsObject, expectedMessage: String): scalatest.Assertion =
      assertBadRequestAndMessage(sender.postJson("/transactions/broadcast", json), expectedMessage)

    val timestamp = System.currentTimeMillis
    val jsonV1 = Json.obj(
      "type"            -> CreateAliasTransaction.typeId,
      "senderPublicKey" -> "8LbAU5BSrGkpk5wbjLMNjrbc9VzN9KBBYv9X8wGpmAJT",
      "alias"           -> "alias",
      "fee"             -> 100000,
      "timestamp"       -> timestamp,
      "signature"       -> "A" * 64
    )

    assertBroadcastBadJson(jsonV1, "invalid signature")

    val jsonV2 = Json.obj(
      "type"            -> CreateAliasTransaction.typeId,
      "version"         -> 2,
      "senderPublicKey" -> "8LbAU5BSrGkpk5wbjLMNjrbc9VzN9KBBYv9X8wGpmAJT",
      "alias"           -> "alias",
      "fee"             -> 100000,
      "timestamp"       -> timestamp,
      "proofs"          -> List("A" * 64)
    )

    assertBroadcastBadJson(jsonV2, "Proof doesn't validate")

    for (j <- List(jsonV1, jsonV2)) {
      assertBroadcastBadJson(j - "type", "failed to parse json message")
      assertBroadcastBadJson(j - "type" + ("type" -> Json.toJson(88)), "Bad transaction type")
      assertBroadcastBadJson(j - "chainId" + ("chainId" -> Json.toJson(123)), "Invalid chain id")
      assertBroadcastBadJson(j - "alias", "failed to parse json message")
    }
  }

  test("/transactions/sign should produce issue/reissue/burn/transfer transactions that are good for /transactions/broadcast") {
    for (v <- supportedVersions) {
      val isProof = Option(v).nonEmpty
      val issueId = signBroadcastAndCalcFee(
        Json.obj(
          "type"        -> IssueTransaction.typeId,
          "name"        -> "Gigacoin",
          "quantity"    -> 100.waves,
          "description" -> "Gigacoin",
          "sender"      -> firstAddress,
          "decimals"    -> 8,
          "reissuable"  -> true
        ),
        usesProofs = isProof,
        version = v
      )

      signBroadcastAndCalcFee(
        Json.obj("type" -> ReissueTransaction.typeId, "quantity" -> 200.waves, "assetId" -> issueId, "sender" -> firstAddress, "reissuable" -> false),
        usesProofs = isProof,
        version = v
      )

      signBroadcastAndCalcFee(Json.obj("type" -> BurnTransaction.typeId, "quantity" -> 0, "assetId" -> issueId, "sender" -> firstAddress),
                              usesProofs = isProof,
                              version = v)

      signBroadcastAndCalcFee(Json.obj("type" -> BurnTransaction.typeId, "quantity" -> 100.waves, "assetId" -> issueId, "sender" -> firstAddress),
                              usesProofs = isProof,
                              version = v)

      signBroadcastAndCalcFee(
        Json.obj(
          "type"       -> TransferTransaction.typeId,
          "sender"     -> firstAddress,
          "recipient"  -> secondAddress,
          "assetId"    -> issueId,
          "amount"     -> 1.waves,
          "attachment" -> Base58.encode("asset transfer".getBytes("UTF-8"))
        ),
        usesProofs = isProof,
        version = v
      )
    }
  }

  test("/transactions/sign should produce transfer transaction that is good for /transactions/broadcast") {
    for (v <- supportedVersions) {
      signBroadcastAndCalcFee(
        Json.obj(
          "type"       -> TransferTransaction.typeId,
          "sender"     -> firstAddress,
          "recipient"  -> secondAddress,
          "amount"     -> transferAmount,
          "attachment" -> Base58.encode("falafel".getBytes("UTF-8"))
        ),
        usesProofs = Option(v).nonEmpty,
        version = v
      )
    }
  }

  test("/transactions/sign should produce mass transfer transaction that is good for /transactions/broadcast") {
    signBroadcastAndCalcFee(
      Json.obj(
        "type"       -> MassTransferTransaction.typeId,
        "version"    -> 1,
        "sender"     -> firstAddress,
        "transfers"  -> Json.toJson(Seq(Transfer(secondAddress, 1.waves), Transfer(thirdAddress, 2.waves))),
        "attachment" -> Base58.encode("masspay".getBytes("UTF-8"))
      ),
      usesProofs = true,
      version = 1
    )
  }

  test("/transactions/sign should produce lease/cancel transactions that are good for /transactions/broadcast") {
    for (v <- supportedVersions) {
      val isProof = Option(v).nonEmpty
      val leaseId =
        signBroadcastAndCalcFee(
          Json.obj("type" -> LeaseTransaction.typeId, "sender" -> firstAddress, "amount" -> leasingAmount, "recipient" -> secondAddress),
          usesProofs = isProof,
          version = v)

      signBroadcastAndCalcFee(Json.obj("type" -> LeaseCancelTransaction.typeId, "sender" -> firstAddress, "txId" -> leaseId),
                              usesProofs = isProof,
                              version = v)
    }
  }

  test("/transactions/sign should produce alias transaction that is good for /transactions/broadcast") {
    for (v <- supportedVersions) {
      val isProof = Option(v).nonEmpty
      val rnd     = Random.alphanumeric.take(9).mkString.toLowerCase
      signBroadcastAndCalcFee(Json.obj("type" -> CreateAliasTransaction.typeId, "sender" -> firstAddress, "alias" -> s"myalias$rnd"),
                              usesProofs = isProof,
                              version = v)
    }
  }

  test("/transactions/sign should produce data transaction that is good for /transactions/broadcast") {
    signBroadcastAndCalcFee(
      Json.obj(
        "type"    -> DataTransaction.typeId,
        "version" -> 1,
        "sender"  -> firstAddress,
        "data" -> List(
          IntegerDataEntry("int", 923275292849183L),
          BooleanDataEntry("bool", value = true),
          BinaryDataEntry("blob", ByteStr(Array.tabulate(445)(_.toByte))),
          StringDataEntry("str", "AAA-AAA")
        )
      ),
      usesProofs = true,
      version = 1
    )
  }

  test("/transactions/sign should produce script transaction that is good for /transactions/broadcast") {
    signBroadcastAndCalcFee(
      Json.obj(
        "type"    -> SetScriptTransaction.typeId,
        "version" -> 1,
        "sender"  -> firstAddress,
        "script"  -> ""
      ),
      usesProofs = true,
      version = 1
    )
  }

  test("/transactions/sign should produce sponsor transactions that are good for /transactions/broadcast") {
    for (v <- supportedVersions) {
      val isProof = Option(v).nonEmpty

      val assetId = signBroadcastAndCalcFee(
        Json.obj(
          "type"        -> IssueTransaction.typeId,
          "name"        -> "Sponsored Coin",
          "quantity"    -> 100.waves,
          "description" -> "Sponsored Coin",
          "sender"      -> firstAddress,
          "decimals"    -> 2,
          "reissuable"  -> false
        ),
        usesProofs = isProof,
        version = v
      )

      signBroadcastAndCalcFee(
        Json.obj(
          "type"                 -> SponsorFeeTransaction.typeId,
          "version"              -> 1,
          "sender"               -> firstAddress,
          "assetId"              -> assetId,
          "minSponsoredAssetFee" -> 100
        ),
        usesProofs = true,
        version = 1
      )

      signBroadcastAndCalcFee(
        Json.obj(
          "type"                 -> SponsorFeeTransaction.typeId,
          "version"              -> 1,
          "sender"               -> firstAddress,
          "assetId"              -> assetId,
          "minSponsoredAssetFee" -> JsNull
        ),
        usesProofs = true,
        version = 1
      )
    }
  }

  test("/transactions/sign/{signerAddress} should sign a transaction by key of signerAddress") {
    val json = Json.obj(
      "type"      -> TransferTransaction.typeId,
      "sender"    -> firstAddress,
      "recipient" -> secondAddress,
      "fee"       -> minFee,
      "amount"    -> transferAmount
    )

    val signedRequestResponse = sender.postJsonWithApiKey(s"/transactions/sign/$thirdAddress", json)
    assert(signedRequestResponse.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
    val signedRequestJson = Json.parse(signedRequestResponse.getResponseBody)
    val signedRequest     = signedRequestJson.as[SignedTransferV1Request]
    assert(PublicKey.fromBase58String(signedRequest.senderPublicKey).explicitGet().stringRepr == firstAddress)
    assert(signedRequest.recipient == secondAddress)
    assert(signedRequest.fee == minFee)
    assert(signedRequest.amount == transferAmount)
    val signature  = Base58.tryDecodeWithLimit((signedRequestJson \ "signature").as[String]).get
    val tx         = signedRequest.toTx.explicitGet()
    val keyPair = pkByAddress(thirdAddress)
    assert(crypto.verify(signature, tx.bodyBytes(), keyPair.publicKey))
  }

  test("/transactions/broadcast should produce ExchangeTransaction with custom asset") {
    val issueTx = signBroadcastAndCalcFee(
      Json.obj(
        "type"        -> IssueTransaction.typeId,
        "name"        -> "ExchangeCoin",
        "quantity"    -> 1000 * someAssetAmount,
        "description" -> "ExchangeCoin Description",
        "sender"      -> firstAddress,
        "decimals"    -> 2,
        "reissuable"  -> true
      ),
      usesProofs = false,
      version = 1
    )

    val assetId = extractAssetId(issueTx).get

    val transactionV1versions = (1: Byte, 1: Byte, 1: Byte) // in ExchangeTransactionV1 only orders V1 are supported
    val transactionV2versions = for {
      o1ver <- 1 to 3
      o2ver <- 1 to 3
    } yield (o1ver.toByte, o2ver.toByte, 2.toByte)

    val versionsWithWavesFee =
      (transactionV1versions +: transactionV2versions)
        .map { case (o1ver, o2ver, tver) => (o1ver, o2ver, tver, Waves, Waves) }

    val versionsWithAssetFee = for {
      o2ver <- 1 to 3
      buyMatcherFeeAssetId  = assetId
      sellMatcherFeeAssetId = Waves
    } yield (3.toByte, o2ver.toByte, 2.toByte, buyMatcherFeeAssetId, sellMatcherFeeAssetId)

    for ((o1ver, o2ver, tver, matcherFeeOrder1, matcherFeeOrder2) <- versionsWithWavesFee ++ versionsWithAssetFee) {
      val buyer               = pkByAddress(firstAddress)
      val seller              = pkByAddress(secondAddress)
      val matcher             = pkByAddress(thirdAddress)
      val ts                  = ntpTime.correctedTime()
      val expirationTimestamp = ts + Order.MaxLiveTime
      val buyPrice            = 1 * Order.PriceConstant
      val sellPrice           = (0.50 * Order.PriceConstant).toLong
      val mf                  = 300000L
      val buyAmount           = 2
      val sellAmount          = 3
      val assetPair           = AssetPair.createAssetPair("WAVES", issueTx).get
      val buy                 = Order.buy(buyer, matcher, assetPair, buyAmount, buyPrice, ts, expirationTimestamp, mf, o1ver, matcherFeeOrder1)
      val sell                = Order.sell(seller, matcher, assetPair, sellAmount, sellPrice, ts, expirationTimestamp, mf, o2ver, matcherFeeOrder2)

      val amount = math.min(buy.amount, sell.amount)
      val tx =
        if (tver == 1) {
          ExchangeTransactionV1
            .create(
              matcher = matcher,
              buyOrder = buy.asInstanceOf[OrderV1],
              sellOrder = sell.asInstanceOf[OrderV1],
              amount = amount,
              price = sellPrice,
              buyMatcherFee = (BigInt(mf) * amount / buy.amount).toLong,
              sellMatcherFee = (BigInt(mf) * amount / sell.amount).toLong,
              fee = mf,
              timestamp = ts
            )
            .explicitGet()
            .json()
        } else {
          ExchangeTransactionV2
            .create(
              matcher = matcher,
              buyOrder = buy,
              sellOrder = sell,
              amount = amount,
              price = sellPrice,
              buyMatcherFee = (BigInt(mf) * amount / buy.amount).toLong,
              sellMatcherFee = (BigInt(mf) * amount / sell.amount).toLong,
              fee = mf,
              timestamp = ts
            )
            .explicitGet()
            .json()
        }
      val s = sell.getReceiveAmount(amount, sellPrice).right.get
      log.info(s"SELLER: ${s}")
      log.info(s"BUYER: ${buy.getReceiveAmount(amount, sellPrice).right.get}")

      val txId = sender.signedBroadcast(tx).id
      sender.waitForTransaction(txId)
      assertBadRequestAndMessage(sender.signedBroadcast(tx), "is already in the state on a height")
    }
  }

  private def signBroadcastAndCalcFee(json: JsObject, usesProofs: Boolean, version: Byte): String = {
    val jsWithPK  = json ++ Json.obj("senderPublicKey" -> sender.publicKey.toString)
    val jsWithFee = jsWithPK ++ Json.obj("fee" -> sender.calculateFee(jsWithPK).feeAmount)
    val js        = if (Option(version).isDefined) jsWithFee ++ Json.obj("version" -> version) else jsWithFee
    val rs        = sender.postJsonWithApiKey("/transactions/sign", js)
    assert(rs.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
    val body = Json.parse(rs.getResponseBody)
    val signed: Boolean = if (usesProofs) {
      val proofs = (body \ "proofs").as[Seq[String]]
      proofs.lengthCompare(1) == 0 && proofs.head.nonEmpty
    } else (body \ "signature").as[String].nonEmpty
    assert(signed)

    val validation = sender.postJson("/debug/validate", body)
    assert(validation.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
    val validationTime = (Json.parse(validation.getResponseBody) \ "validationTime").as[Double]
    log.debug(s"Validation time of tx is $validationTime ")

    val rb = sender.postJson("/transactions/broadcast", body)
    assert(rb.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
    val id = (Json.parse(rb.getResponseBody) \ "id").as[String]
    assert(id.nonEmpty)
    sender.waitForTransaction(id)
    id
  }
}
