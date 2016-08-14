package scorex.waves

import org.scalatest.{BeforeAndAfterAll, DoNotDiscover, FunSuite, Matchers}
import play.api.libs.json.Json
import scorex.api.http._
import scorex.waves.http.UnsignedPayment
import scorex.waves.transaction.{ExternalPayment, SignedPayment}

@DoNotDiscover
class WavesAPISpecification extends FunSuite with Matchers with BeforeAndAfterAll {

  import TestingCommons._

  override def beforeAll: Unit = {
    start()
  }

  override def afterAll: Unit = {
    stop()
  }


  test("/waves/create-signed-payment API route checks sender balance") {
    val recipient = "3N5jhcA7R98AUN12ee9pB7unvnAKfzb3nen"
    val timestamp = 1465391445252L
    val amount = 10000000L
    val payment = UnsignedPayment(timestamp, amount, 100000L, recipient, "5JiSMVVvadkKt2K9dDJjiwLiDzuCMmzcHnNuEzct2LiY", 1)
    val json = Json.toJson(payment).toString

    val response = postRequest(us = "/waves/create-signed-payment", body = json)
    println(response.toString)
    assert(response.toString == NoBalance.json.toString)
  }

  test("/waves/external-payment API route can not send to address from another net") {
    val senderPublicKey = "GvXeYd2iFJUNV7KgeGV2cdnScyrEvrr9uPYJeQFtvg21"
    val recipient = "3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J"
    val timestamp = 1465391445252L
    val amount = 10000000000000L
    val payment = ExternalPayment(timestamp, amount, 400L, recipient, senderPublicKey, "1")
    val json = Json.toJson(payment).toString

    val response = postRequest(us = "/waves/external-payment", body = json)
    assert(response.toString == InvalidAddress.json.toString)
  }

  test("/waves/external-payment can not send from suspendedAddresses") {
    val senderPublicKey = "5JiSMVVvadkKt2K9dDJjiwLiDzuCMmzcHnNuEzct2LiY"
    val recipient = "3N5jhcA7R98AUN12ee9pB7unvnAKfzb3nen"
    val timestamp = 1465391445252L
    val amount = 10000000000000L
    val payment = ExternalPayment(timestamp, amount, 100000L, senderPublicKey, recipient, "1")
    val json = Json.toJson(payment).toString
    val response = postRequest(us = "/waves/external-payment", body = json)
    assert(response.toString == InvalidSender.json.toString)
  }

  test("/waves/broadcast-signed-payment API route can not send to address from another net") {

    val senderPublicKey = "GvXeYd2iFJUNV7KgeGV2cdnScyrEvrr9uPYJeQFtvg21"
    val recipient = "3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J"
    val timestamp = 1465391445252L
    val amount = 10000000000000L
    val payment = SignedPayment(timestamp, amount, 400L, recipient, senderPublicKey, "", "1")
    val json = Json.toJson(payment).toString

    val response = postRequest(us = "/waves/broadcast-signed-payment", body = json)
    assert(response.toString == InvalidAddress.json.toString)
  }

  test("/waves/broadcast-signed-payment can not send from suspendedAddresses") {
    val senderPublicKey = "5JiSMVVvadkKt2K9dDJjiwLiDzuCMmzcHnNuEzct2LiY"
    val recipient = "3N5jhcA7R98AUN12ee9pB7unvnAKfzb3nen"
    val timestamp = 1465391445252L
    val amount = 10000000000000L
    val payment = SignedPayment(timestamp, amount, 100000L, recipient, senderPublicKey, "", "1")
    val json = Json.toJson(payment).toString
    val response = postRequest(us = "/waves/broadcast-signed-payment", body = json)
    assert(response.toString == InvalidSender.json.toString)
  }

  test("/waves/broadcast-signed-payment can not send tx with small fee") {
    val senderPublicKey = "GvXeYd2iFJUNV7KgeGV2cdnScyrEvrr9uPYJeQFtvg21"
    val recipient = "3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J"
    val timestamp = 1465391445252L
    val amount = 10000000000000L
    val payment = SignedPayment(timestamp, amount, 4L, recipient, senderPublicKey, "", "1")
    val json = Json.toJson(payment).toString

    val response = postRequest(us = "/waves/broadcast-signed-payment", body = json)
    assert(response.toString == InsufficientFee.json.toString)
  }

  test("/waves/address returns correct CORS header") {
    val response = postRequestWithResponse(
      us = "/waves/address", body = "GvXeYd2iFJUNV7KgeGV2cdnScyrEvrr9uPYJeQFtvg21")
    assert(response.getHeaders("Access-Control-Allow-Origin").size == 1)
  }

  test("/waves/* API returns correct CORS header") {
    val urls = List("/waves/address",
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