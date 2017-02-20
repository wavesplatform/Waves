package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58

class AddressesAPISpecification extends FunSuite with Matchers with scorex.waves.TestingCommons {

  private def accounts = application.wallet.privateKeyAccounts()

  private def addresses = accounts.map(_.address)

  private def account = accounts.head

  private def address = account.address

  override def beforeAll(): Unit = {
    if (application.wallet.privateKeyAccounts().size < 10) application.wallet.generateNewAccounts(10)
    super.beforeAll()
  }

  test("/addresses/seq/{from}/{to} API route") {
    val responded = GET.request("/addresses/seq/1/4").as[List[String]]
    responded.size shouldBe 3
    responded.foreach(a => addresses should contain(a))

    val r2 = GET.request("/addresses/seq/5/9").as[List[String]]
    r2.size shouldBe 4
    r2.foreach(a => addresses should contain(a))
    responded.intersect(r2).isEmpty shouldBe true
  }

  test("/addresses/validate/{address} API route") {
    val toCheck: Seq[(String, Boolean)] = ("wrongA", false) +: addresses.map(a => (a, true))
    toCheck.foreach { a =>
      val response = GET.request(s"/addresses/validate/${a._1}")
      (response \ "address").as[String] shouldBe a._1
      (response \ "valid").as[Boolean] shouldBe a._2
    }
  }

  test("/addresses/seed/{address} API route") {
    val path = s"/addresses/seed/${account.address}"
    GET.incorrectApiKeyTest(path)

    val response = GET.request(us = path, headers = Map("api_key" -> "test"))
    (response \ "address").as[String] shouldBe account.address
    (response \ "seed").as[String] shouldBe Base58.encode(account.seed)
  }

  test("/addresses/balance/{address} API route") {
    val response = GET.request(s"/addresses/balance/$address")
    (response \ "address").as[String] shouldBe address
    (response \ "confirmations").as[Int] shouldBe 0
    (response \ "balance").as[Long] should be >= 0L
  }

  test("POST /addresses/sign/{address} API route") {
    val message = "test"
    val req = POST.request(s"/addresses/sign/$address", body = message)
    (req \ "message").as[String] shouldBe Base58.encode(message.getBytes)
    val pubkey = (req \ "publicKey").asOpt[String].flatMap(Base58.decode(_).toOption)
    val signature = (req \ "signature").asOpt[String].flatMap(Base58.decode(_).toOption)
    pubkey.isDefined shouldBe true
    signature.isDefined shouldBe true

    EllipticCurveImpl.verify(signature.get, message.getBytes, pubkey.get)

    POST.incorrectApiKeyTest(s"/addresses/sign/$address")
  }

  test("/addresses/balance/{address}/{confirmations} API route") {
    val confirmations = Math.min(3, application.blockStorage.state.stateHeight)
    val response = GET.request(s"/addresses/balance/$address/$confirmations")
    (response \ "address").as[String] shouldBe address
    (response \ "confirmations").as[Int] shouldBe confirmations
    (response \ "balance").as[Long] should be >= 0L
  }

  test("DELETE /addresses/{address} API route") {
    val address = accounts.last.address
    DELETE.incorrectApiKeyTest(s"/addresses/$address")

    (DELETE.request(s"/addresses/$address") \ "deleted").as[Boolean] shouldBe true
    addresses.contains(address) shouldBe false
  }

  test("POST /addresses/verifyText/{address} API route") {
    val address = "3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K" /*"3MbWTyn6Tg7zL6XbdN8TLcFMfhWX76fGNCz"*/
    POST.incorrectApiKeyTest(s"/addresses/verifyText/$address")

    val signed =
      """{
        |  "message": "test",
        |  "publickey": "DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ",
        |  "signature": "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
        |}""".stripMargin
    (POST.requestJson(s"/addresses/verifyText/$address", body = signed) \ "valid").as[Boolean] shouldBe true

    val incorrect =
      """{
        |  "message": "test2",
        |  "publickey": "DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ",
        |  "signature": "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
        |}""".stripMargin
    (POST.requestJson(s"/addresses/verifyText/$address", body = incorrect) \ "valid").as[Boolean] shouldBe false
  }

  test("POST /addresses/signText/{address} API route") {
    val message = "test"
    val req = POST.request(s"/addresses/signText/$address", body = message)
    (req \ "message").as[String] shouldBe message
    val pubkey = (req \ "publicKey").asOpt[String].flatMap(Base58.decode(_).toOption)
    val signature = (req \ "signature").asOpt[String].flatMap(Base58.decode(_).toOption)
    pubkey.isDefined shouldBe true
    signature.isDefined shouldBe true

    EllipticCurveImpl.verify(signature.get, message.getBytes, pubkey.get)

    POST.incorrectApiKeyTest(s"/addresses/signText/$address")
  }

  test("POST /addresses API route") {
    (POST.request("/addresses") \ "address").asOpt[String].flatMap(Base58.decode(_).toOption).isDefined shouldBe true
    val add = "/addresses"

    POST.incorrectApiKeyTest("/addresses")
  }

  test("POST /addresses  API route returns correct CORS for invalid api key") {
    val response = POST.requestRaw(us = "/addresses", headers = Map("api_key" -> "invalid"))
    assert(response.getHeaders("Access-Control-Allow-Origin").size == 1)
  }

  test("/addresses/ API route") {
    val ads: Seq[String] = addresses
    val response = GET.request("/addresses")
    response.as[List[String]] shouldBe ads
  }

  test("POST /addresses/verify/{address} API route") {
    val address = "3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K"
    POST.incorrectApiKeyTest(s"/addresses/verify/$address")

    val signed =
      """{
        |  "message": "3yZe7d",
        |  "publickey": "DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ",
        |  "signature": "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
        |}""".stripMargin
    (POST.requestJson(s"/addresses/verify/$address", body = signed) \ "valid").as[Boolean] shouldBe true

    val incorrect =
      """{
        |  "message": "3yZe7dd",
        |  "publickey": "DZUxn4pC7QdYrRqacmaAJghatvnn1Kh1mkE2scZoLuGJ",
        |  "signature": "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
        |}""".stripMargin
    (POST.requestJson(s"/addresses/verify/$address", body = incorrect) \ "valid").as[Boolean] shouldBe false
  }

}
