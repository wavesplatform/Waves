package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderJson._
import com.wavesplatform.transaction.smart.Verifier
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json._

class OrderJsonSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Read Order from json") {
    val keyPair   = KeyPair("123".getBytes("UTF-8"))
    val pubKeyStr = Base58.encode(keyPair.publicKey)

    val json = Json.parse(s"""
        {
          "senderPublicKey": "$pubKeyStr",
          "matcherPublicKey": "DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ",
          "assetPair": {
            "amountAsset": "29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b",
            "priceAsset": "GEtBMkg419zhDiYRXKwn2uPcabyXKqUqj4w3Gcs1dq44"
          },
          "orderType": "buy",
          "amount": 0,
          "matcherFee": 0,
          "price": 0,
          "timestamp": 0,
          "expiration": 0,
          "signature": "signature"
        } """)

    json.validate[Order] match {
      case JsError(e) =>
        fail("Error: " + e.toString())
      case JsSuccess(o, _) =>
        o.senderPublicKey shouldBe keyPair.publicKey
        o.matcherPublicKey shouldBe PublicKey(Base58.tryDecodeWithLimit("DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ").get)
        o.assetPair.amountAsset.compatId.get shouldBe ByteStr.decodeBase58("29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b").get
        o.assetPair.priceAsset.compatId.get shouldBe ByteStr.decodeBase58("GEtBMkg419zhDiYRXKwn2uPcabyXKqUqj4w3Gcs1dq44").get
        o.price shouldBe 0
        o.amount shouldBe 0
        o.matcherFee shouldBe 0
        o.timestamp shouldBe 0
        o.expiration shouldBe 0
        o.signature shouldBe Base58.tryDecodeWithLimit("signature").get
    }

    val jsonOV3 = Json.parse(s"""
        {
          "version": 3,
          "senderPublicKey": "$pubKeyStr",
          "matcherPublicKey": "DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ",
          "assetPair": {
            "amountAsset": "29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b",
            "priceAsset": "GEtBMkg419zhDiYRXKwn2uPcabyXKqUqj4w3Gcs1dq44"
          },
          "orderType": "buy",
          "amount": 0,
          "matcherFee": 0,
          "price": 0,
          "timestamp": 0,
          "expiration": 0,
          "signature": "signature",
          "matcherFeeAssetId": "29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b"
        } """)

    jsonOV3.validate[Order] match {
      case JsError(e) =>
        fail("Error: " + e.toString())
      case JsSuccess(o, _) =>
        o.senderPublicKey shouldBe keyPair.publicKey
        o.matcherPublicKey shouldBe PublicKey(Base58.tryDecodeWithLimit("DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ").get)
        o.assetPair.amountAsset shouldBe IssuedAsset(ByteStr.decodeBase58("29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b").get)
        o.assetPair.priceAsset shouldBe IssuedAsset(ByteStr.decodeBase58("GEtBMkg419zhDiYRXKwn2uPcabyXKqUqj4w3Gcs1dq44").get)
        o.price shouldBe 0
        o.amount shouldBe 0
        o.matcherFee shouldBe 0
        o.timestamp shouldBe 0
        o.expiration shouldBe 0
        o.signature shouldBe Base58.tryDecodeWithLimit("signature").get
        o.matcherFeeAssetId shouldBe IssuedAsset(ByteStr.decodeBase58("29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b").get)
    }
  }

  property("Read Order without sender and matcher PublicKey") {
    val json = Json.parse("""
        {
          "senderPublicKey": " ",
          "spendAssetId": "string",
          "receiveAssetId": "string",
          "amount": 0,
          "matcherFee": 0,
          "price": 0,
          "timestamp": 0,
          "expiration": 0,
          "signature": "signature"
        } """)

    json.validate[Order] match {
      case e: JsError =>
        val paths = e.errors.map(_._1)
        paths should contain allOf (JsPath \ "matcherPublicKey", JsPath \ "senderPublicKey")
      case _ =>
        fail("Should be JsError")
    }
  }

  val base58Str     = "DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ"
  val json: JsValue = Json.parse(s"""
    {
      "sender": "$base58Str",
      "wrong_sender": "0abcd",
      "wrong_long": "12e",
      "publicKey": "$base58Str",
      "wrong_publicKey": "0abcd"
    }
    """)

  property("Json Reads Base58") {
    val sender = (json \ "sender").as[Option[Array[Byte]]]
    sender.get shouldBe Base58.tryDecodeWithLimit(base58Str).get

    (json \ "wrong_sender").validate[Array[Byte]] shouldBe a[JsError]
  }

  property("Json Reads PublicKey") {
    val publicKey = (json \ "publicKey").as[PublicKey]
    publicKey.bytes shouldBe PublicKey(Base58.tryDecodeWithLimit(base58Str).get).bytes

    (json \ "wrong_publicKey").validate[PublicKey] match {
      case e: JsError =>
        e.errors.head._2.head.message shouldBe "error.incorrectAccount"
      case _ => fail("Should be JsError")
    }
  }

  property("Parse signed Order") {
    forAll(orderGen) { order =>
      val json = order.json()
      json.validate[Order] match {
        case e: JsError =>
          fail("Error: " + JsError.toJson(e).toString())
        case s: JsSuccess[Order] =>
          val o = s.get
          o.json().toString() should be(json.toString())
          Verifier.verifyAsEllipticCurveSignature(o) shouldBe 'right
      }
    }
  }

  property("Read Order with empty assetId") {
    val pk        = KeyPair("123".getBytes("UTF-8"))
    val pubKeyStr = Base58.encode(pk.publicKey)

    def mkJson(priceAsset: String): String = s"""
        {
          "senderPublicKey": "$pubKeyStr",
          "matcherPublicKey": "DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ",
           "assetPair": {
             "amountAsset": "",
             "priceAsset": $priceAsset
           },
          "orderType": "sell",
          "amount": 0,
          "matcherFee": 0,
          "price": 0,
          "timestamp": 0,
          "expiration": 0,
          "signature": "signature"
        } """

    val jsons = Seq(""" "" """, "null", """ "WAVES" """).map { x =>
      x -> mkJson(x)
    }

    jsons.foreach {
      case (priceAssetStr, rawJson) =>
        withClue(priceAssetStr) {
          Json.parse(rawJson).validate[Order] match {
            case e: JsError =>
              fail("Error: " + JsError.toJson(e).toString())
            case s: JsSuccess[Order] =>
              val o = s.get
              o.assetPair.amountAsset shouldBe Waves
              o.assetPair.priceAsset shouldBe Waves
          }
        }
    }
  }
}
