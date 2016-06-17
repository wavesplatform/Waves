package scorex.waves

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import scorex.api.http.{InvalidAddress, InvalidSender}
import scorex.waves.transaction.ExternalPayment

class WavesAPISpecification extends FunSuite with Matchers {

  import TestingCommons._

  test("/waves/external-payment API route can not send to address from another net") {

    val senderPublicKey = "GvXeYd2iFJUNV7KgeGV2cdnScyrEvrr9uPYJeQFtvg21"
    val recipient = "3MqS3mVY4Yr4HoTdpWiEaq9phwbaoWS2W6A"
    val timestamp = 1465391445252L
    val amount = 10000000000000L
    val payment = ExternalPayment(timestamp, amount, 1L, senderPublicKey, recipient, "1")
    val json = Json.toJson(payment).toString

    val response = postRequest(us = "/waves/external-payment", body = json)
    assert(response.toString == InvalidAddress.json.toString)
  }

  test("/wave/external-payment can not send from suspendedAddresses") {
    val senderPublicKey = "4ZADu6W3ZBfNv1LLQCkoHAdiVfvatZNNB6reSyo7c9qJ"
    val recipient = "3N5jhcA7R98AUN12ee9pB7unvnAKfzb3nen"
    val timestamp = 1465391445252L
    val amount = 10000000000000L
    val payment = ExternalPayment(timestamp, amount, 1L, senderPublicKey, recipient, "1")
    val json = Json.toJson(payment).toString
    val response = postRequest(us = "/waves/external-payment", body = json)
    assert(response.toString == InvalidSender.json.toString)
  }

}