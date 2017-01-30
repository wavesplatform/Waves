package scorex.waves

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import scorex.account.{Account, PublicKeyAccount}
import scorex.api.http._
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.waves.http.UnsignedPayment
import scorex.waves.transaction.{ExternalPayment, SignedPayment}
import scorex.transaction.TypedTransaction._

class WavesAPISpecification extends FunSuite with Matchers with scorex.waves.TestingCommons {

  test("/waves/create-signed-payment API route checks sender balance") {
    val recipient = "3N5jhcA7R98AUN12ee9pB7unvnAKfzb3nen"
    val timestamp = 1465391445252L
    val amount = 10000000L
    val payment = UnsignedPayment(timestamp, amount, 100000L, recipient, "5JiSMVVvadkKt2K9dDJjiwLiDzuCMmzcHnNuEzct2LiY", 1)
    val paymentJson = Json.toJson(payment)
    val json = paymentJson.toString

    val response = postRequest(us = "/waves/create-signed-payment", body = json)
    assert(response \ "timestamp" == paymentJson \ "timestamp")
    assert(response \ "amount" == paymentJson \ "amount")
    assert(response \ "fee" == paymentJson \ "fee")
    assert(response \ "recipient" == paymentJson \ "recipient")
    assert((response \ "signature").toOption.isDefined)
  }

  ignore("/waves/external-payment API route can not send to address from another net") {
    val senderPublicKey = new PublicKeyAccount(Base58.decode("GvXeYd2iFJUNV7KgeGV2cdnScyrEvrr9uPYJeQFtvg21").get)
    val recipient = new Account("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J")
    val timestamp = 1465391445252L
    val amount = 10000000000000L
    val signature = Array.fill(SignatureLength)(0.toByte)
    val payment = ExternalPayment(timestamp, amount, 400L, senderPublicKey, recipient, signature)
    val json = Json.toJson(payment).toString

    val response = postRequest(us = "/waves/external-payment", body = json)
    assert(response.toString == InvalidAddress.json.toString)
  }

  test("/waves/external-payment API route can not send to address with invalid length 'recipient' field") {
    val senderPublicKey = new PublicKeyAccount(Base58.decode("GvXeYd2iFJUNV7KgeGV2cdnScyrEvrr9uPYJeQFtvg21").get)
    val recipient = new Account("3PBWXDFUc86N2EQxKJmW8eFco65xTy")
    val timestamp = 1465391445252L
    val amount = 10000000000000L
    val signature = Array.fill(SignatureLength)(0.toByte)
    val payment = ExternalPayment(timestamp, amount, 400L, senderPublicKey, recipient, signature)
    val json = Json.toJson(payment).toString

    val response = postRequest(us = "/waves/external-payment", body = json)
    assert(response.toString == InvalidRecipient.json.toString)
  }

  test("/waves/external-payment API route can not send to address with invalid length 'senderPublicKey' field") {
    val senderPublicKey = new PublicKeyAccount(Base58.decode("GvXeYd2iFJUNV7KgeGV2cdnScyrEvrr9uPYJeQF").get)
    val recipient = new Account("3N1hV1nYsBqJeHQfhEbjhndeLzYFavDsQxM")
    val timestamp = 1465391445252L
    val amount = 10000000000000L
    val signature = Array.fill(SignatureLength)(0.toByte)
    val payment = ExternalPayment(timestamp, amount, 400L, senderPublicKey, recipient, signature)
    val json = Json.toJson(payment).toString

    val response = postRequest(us = "/waves/external-payment", body = json)
    assert(response.toString == InvalidSender.json.toString)
  }

  test("/waves/external-payment API route can not send to address with invalid length 'signature' field") {
    val senderPublicKey = new PublicKeyAccount(Base58.decode("GvXeYd2iFJUNV7KgeGV2cdnScyrEvrr9uPYJeQFtvg21").get)
    val recipient = new Account("3N1hV1nYsBqJeHQfhEbjhndeLzYFavDsQxM")
    val timestamp = 1465391445252L
    val amount = 10000000000000L
    val signature = Array.fill(SignatureLength - 1)(0.toByte)
    val payment = ExternalPayment(timestamp, amount, 400L, senderPublicKey, recipient, signature)
    val json = Json.toJson(payment).toString

    val response = postRequest(us = "/waves/external-payment", body = json)
    assert(response.toString == InvalidSignature.json.toString)
  }

  // todo move to something else test?
  test("API route can be called with oversized request") {
    val senderPublicKey = new PublicKeyAccount(Base58.decode("GvXeYd2iFJUNV7KgeGV2cdnScyrEvrr9uPYJeQFtvg21").get)
    val recipient = new Account("3N1hV1nYsBqJeHQfhEbjhndeLzYFavDsQxM")
    val timestamp = 1465391445252L
    val amount = 10000000000000L
    // see application.conf: http.server.parsing.max-content-length = 1m
    val signature = Array.fill(1 * 1024 * 1024 + 1)(0.toByte)
    val payment = ExternalPayment(timestamp, amount, 400L, senderPublicKey, recipient, signature)
    val json = Json.toJson(payment).toString

    val response = postRequestWithResponse(us = "/waves/external-payment", body = json)
    assert(response.getStatusCode == 405)
  }

  ignore("/waves/broadcast-signed-payment API route can not send to address from another net") {

    val senderPublicKey = new PublicKeyAccount(Base58.decode("GvXeYd2iFJUNV7KgeGV2cdnScyrEvrr9uPYJeQFtvg21").get)
    val recipient = new Account("3MyViFvajzYyPn7Y4EWWBBsoSCaBdrCZSfw")
    val timestamp = 1465391445252L
    val amount = 10000000000000L
    val signature = Array.fill(SignatureLength)(0.toByte)
    val payment = SignedPayment(timestamp, amount, 400L, recipient, senderPublicKey, "", Base58.encode(signature))
    val json = Json.toJson(payment).toString

    val response = postRequest(us = "/waves/broadcast-signed-payment", body = json)
    assert(response.toString == InvalidAddress.json.toString)
  }

  test("/waves/broadcast-signed-payment can not send tx with small fee") {
    val senderPublicKey = new PublicKeyAccount(Base58.decode("GvXeYd2iFJUNV7KgeGV2cdnScyrEvrr9uPYJeQFtvg21").get)
    val recipient = new Account("3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx")
    val timestamp = 1465391445252L
    val amount = 10000000000000L
    val signature = Array.fill(SignatureLength)(0.toByte)
    val payment = SignedPayment(timestamp, amount, 4L, recipient, senderPublicKey, "", Base58.encode(signature))
    val json = Json.toJson(payment).toString

    val response = postRequest(us = "/waves/broadcast-signed-payment", body = json)
    assert(response.toString == InsufficientFee.json.toString)
  }

  test("/waves/* API returns correct CORS header") {
    val urls = List(
      "/waves/broadcast-signed-payment",
      "/waves/create-signed-payment",
      "/waves/external-payment",
      "/waves/payment",
      "/waves/payment/signature")
    urls.foreach {
      url => {
        val response = postRequestWithResponse(us = url, body = "")
        assert(response.getHeaders("Access-Control-Allow-Origin").size == 1, url)
      }
    }
  }
}
