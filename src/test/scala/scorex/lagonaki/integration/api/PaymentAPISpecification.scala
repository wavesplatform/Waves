package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}
import scorex.lagonaki.TransactionTestingCommons
import scorex.lagonaki.integration.TestLock

class PaymentAPISpecification extends FunSuite with TestLock with Matchers with TransactionTestingCommons {

  import scorex.lagonaki.TestingCommons._

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    stopGeneration(applications)
  }

  test("POST /payment API route") {
    POST.incorrectApiKeyTest("/payment")
    val s = accounts.head.address
    val r = accounts.last.address
    val amount = 2
    val fee = 1

    val json = "{\"amount\":" + amount + ",\"fee\":" + fee + ",\"sender\":\"" + s + "\",\"recipient\":\"" + r + "\"\n}"
    val req = POST.request("/payment", body = json)
    (req \ "type").as[Int] shouldBe 2
    (req \ "fee").as[Int] shouldBe 1
    (req \ "amount").as[Int] shouldBe amount
    (req \ "timestamp").asOpt[Long].isDefined shouldBe true
    (req \ "signature").asOpt[String].isDefined shouldBe true
    (req \ "sender").as[String] shouldBe s
    (req \ "recipient").as[String] shouldBe r

  }

  test("POST /payment API route returns correct CORS for invalid api key") {
    val response = POST.requestRaw(us = "/payment", headers = Map("api_key" -> "invalid"))
    assert(response.getHeaders("Access-Control-Allow-Origin").size == 1)
  }
}
