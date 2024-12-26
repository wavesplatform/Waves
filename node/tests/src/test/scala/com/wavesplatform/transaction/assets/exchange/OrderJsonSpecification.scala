package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderJson.*
import com.wavesplatform.transaction.smart.Verifier
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.utils.{EthEncoding, EthHelpers, JsonMatchers}
import play.api.libs.json.*

class OrderJsonSpecification extends PropSpec with JsonMatchers with EthHelpers {

  property("Read Order from json") {
    val keyPair   = KeyPair("123".getBytes("UTF-8"))
    val pubKeyStr = keyPair.publicKey.toString

    val json = Json.parse(s"""
        {
          "senderPublicKey": "$pubKeyStr",
          "matcherPublicKey": "DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ",
          "assetPair": {
            "amountAsset": "29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b",
            "priceAsset": "GEtBMkg419zhDiYRXKwn2uPcabyXKqUqj4w3Gcs1dq44"
          },
          "orderType": "buy",
          "amount": 1,
          "matcherFee": 2,
          "price": 3,
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
        o.price.value shouldBe 3
        o.amount.value shouldBe 1
        o.matcherFee.value shouldBe 2
        o.timestamp shouldBe 0
        o.expiration shouldBe 0
        o.signature shouldBe ByteStr(Base58.decode("signature"))
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
          "amount": 1,
          "matcherFee": 2,
          "price": 3,
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
        o.price.value shouldBe 3
        o.amount.value shouldBe 1
        o.matcherFee.value shouldBe 2
        o.timestamp shouldBe 0
        o.expiration shouldBe 0
        o.signature shouldBe ByteStr(Base58.decode("signature"))
        o.matcherFeeAssetId shouldBe IssuedAsset(ByteStr.decodeBase58("29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b").get)
    }

    val jsonOV4 = Json.parse(s"""
        {
          "version": 4,
          "senderPublicKey": "$pubKeyStr",
          "matcherPublicKey": "DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ",
          "assetPair": {
            "amountAsset": "29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b",
            "priceAsset": "GEtBMkg419zhDiYRXKwn2uPcabyXKqUqj4w3Gcs1dq44"
          },
          "orderType": "buy",
          "amount": 1,
          "matcherFee": 2,
          "price": 3,
          "timestamp": 4,
          "expiration": 5,
          "signature": "signature",
          "matcherFeeAssetId": "29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b"
        } """)

    jsonOV4.validate[Order] match {
      case JsError(e) =>
        fail("Error: " + e.toString())
      case JsSuccess(o, _) =>
        o.id().toString shouldBe "BVJs4ip16nbh2vmuZkQmg8TMbN4vhnRAiACAfffQxSr7"
        o.senderPublicKey shouldBe keyPair.publicKey
        o.matcherPublicKey shouldBe PublicKey(Base58.tryDecodeWithLimit("DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ").get)
        o.assetPair.amountAsset shouldBe IssuedAsset(ByteStr.decodeBase58("29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b").get)
        o.assetPair.priceAsset shouldBe IssuedAsset(ByteStr.decodeBase58("GEtBMkg419zhDiYRXKwn2uPcabyXKqUqj4w3Gcs1dq44").get)
        o.price.value shouldBe 3
        o.amount.value shouldBe 1
        o.matcherFee.value shouldBe 2
        o.timestamp shouldBe 4
        o.expiration shouldBe 5
        o.signature shouldBe ByteStr(Base58.decode("signature"))
        o.matcherFeeAssetId shouldBe IssuedAsset(ByteStr.decodeBase58("29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b").get)
    }

    val jsonOV4WithEthSig = Json.parse(s"""
        {
          "version": 4,
          "matcherPublicKey": "DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ",
          "assetPair": {
            "amountAsset": "29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b",
            "priceAsset": "GEtBMkg419zhDiYRXKwn2uPcabyXKqUqj4w3Gcs1dq44"
          },
          "orderType": "buy",
          "amount": 1,
          "matcherFee": 2,
          "price": 3,
          "timestamp": 4,
          "expiration": 5,
          "signature": "signature",
          "matcherFeeAssetId": "29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b",
          "eip712Signature": "0x40dd06c9f80215612a0397948a10dd82d6a58dda8a256544971e236a95a395ad6b87e75fb58789ece4f2ff7ed380849d120faefce135b6f7ddec9e11df169f971b"
        } """)

    jsonOV4WithEthSig.validate[Order] match {
      case JsError(e) =>
        fail("Error: " + e.toString())
      case JsSuccess(o, _) =>
        o.id().toString shouldBe "FU8kLN9rRXCYjUDVUg914L3rdKNbgqpcfPmzXV7kLSJZ"
        o.withProofs(Proofs.empty).id() shouldNot be(o.id())
        o.senderPublicKey shouldBe PublicKey(
          ByteStr.decodeBase58("4LySXRvAsKTfhvabypvFUwYT3cvUFyZBhzFhq9UUDfzDmM4wDEmu3m5xPSD7iZrm7Zg4mmUXAkEQmodGgrdCAic7").get
        )
        o.matcherPublicKey shouldBe PublicKey(Base58.tryDecodeWithLimit("DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ").get)
        o.assetPair.amountAsset shouldBe IssuedAsset(ByteStr.decodeBase58("29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b").get)
        o.assetPair.priceAsset shouldBe IssuedAsset(ByteStr.decodeBase58("GEtBMkg419zhDiYRXKwn2uPcabyXKqUqj4w3Gcs1dq44").get)
        o.price.value shouldBe 3
        o.amount.value shouldBe 1
        o.matcherFee.value shouldBe 2
        o.timestamp shouldBe 4
        o.expiration shouldBe 5
        o.signature shouldBe ByteStr.empty
        o.matcherFeeAssetId shouldBe IssuedAsset(ByteStr.decodeBase58("29ot86P3HoUZXH1FCoyvff7aeZ3Kt7GqPwBWXncjRF2b").get)
        o.eip712Signature shouldBe Some(
          ByteStr(
            EthEncoding.toBytes(
              "0x40dd06c9f80215612a0397948a10dd82d6a58dda8a256544971e236a95a395ad6b87e75fb58789ece4f2ff7ed380849d120faefce135b6f7ddec9e11df169f971b"
            )
          )
        )
    }
  }

  property("Read Order without sender and matcher PublicKey") {
    val json = Json.parse("""
        {
          "senderPublicKey": " ",
          "spendAssetId": "string",
          "receiveAssetId": "string",
          "amount": 1,
          "matcherFee": 2,
          "price": 3,
          "timestamp": 0,
          "expiration": 0,
          "signature": "signature"
        } """)

    json.validate[Order] match {
      case e: JsError =>
        val paths = e.errors.map(_._1)
        paths should contain.allOf(JsPath \ "matcherPublicKey", JsPath \ "senderPublicKey")
      case _ =>
        fail("Should be JsError")
    }
  }

  val base58Str = "DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ"
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
    publicKey shouldBe PublicKey(Base58.tryDecodeWithLimit(base58Str).get)

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

        case JsSuccess(o: Order, _) =>
          o.json() should matchJson(json)
          Verifier.verifyAsEllipticCurveSignature(o, isRideV6Activated = false).explicitGet()
      }
    }
  }

  property("Read Order with empty assetId") {
    def mkJson(priceAsset: String): String =
      s"""
        {
          "senderPublicKey": "${KeyPair("123".getBytes("UTF-8")).publicKey.toString}",
          "matcherPublicKey": "DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ",
           "assetPair": {
             "amountAsset": "",
             "priceAsset": $priceAsset
           },
          "orderType": "sell",
          "amount": 1,
          "matcherFee": 2,
          "price": 3,
          "timestamp": 0,
          "expiration": 0,
          "signature": "signature"
        } """

    val jsons = Seq(""" "" """, "null", """ "WAVES" """).map { x =>
      x -> mkJson(x)
    }

    jsons.foreach { case (priceAssetStr, rawJson) =>
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
