package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}
import scorex.api.http.ApiKeyNotValid
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.lagonaki.TestingCommons

class AddressesAPISpecification extends FunSuite with Matchers {

  import TestingCommons._

  val wallet = application.wallet
  if (wallet.privateKeyAccounts().size < 10) wallet.generateNewAccounts(10)
  val account = accounts.head
  val address = account.address


  test("/addresses/seq/{from}/{to} API route") {
    val responsed = GET.request("/addresses/seq/1/4").as[List[String]]
    responsed.size shouldBe 3
    responsed.foreach(a => addresses should contain(a))

    val r2 = GET.request("/addresses/seq/5/9").as[List[String]]
    r2.size shouldBe 4
    r2.foreach(a => addresses should contain(a))
    responsed.intersect(r2).isEmpty shouldBe true

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
    val response = GET.request(s"/addresses/seed/${account.address}")
    (response \ "address").as[String] shouldBe account.address
    (response \ "seed").as[String] shouldBe Base58.encode(account.seed)
  }

  test("/addresses/balance/{address} API route") {
    val response = GET.request(s"/addresses/balance/$address")
    (response \ "address").as[String] shouldBe address
    (response \ "confirmations").as[Int] shouldBe 1
    (response \ "balance").as[Long] should be >= 0L
  }

  test("POST /addresses/sign/{address} API route") {
    val message = "test"
    val req = POST.request(s"/addresses/sign/$address", body = message)
    (req \ "message").as[String] shouldBe Base58.encode(message.getBytes)
    val pubkey = (req \ "publickey").asOpt[String].flatMap(Base58.decode(_).toOption)
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

  test("/addresses/generatingbalance/{address} API route") {
    val response = GET.request(s"/addresses/generatingbalance/$address")
    (response \ "address").as[String] shouldBe address
    (response \ "balance").as[Long] should be >= 0L
  }

  test("DELETE /addresses/{address} API route") {
    val address = accounts.last.address
    DELETE.incorrectApiKeyTest(s"/addresses/$address")

    (DELETE.request(s"/addresses/$address") \ "deleted").as[Boolean] shouldBe true
    addresses.contains(address) shouldBe false
  }

  test("POST /addresses/verifyText/{address} API route") {
    val address = "jACSbUoHi4eWgNu6vzAnEx583NwmUAVfS"
    POST.incorrectApiKeyTest(s"/addresses/verifyText/$address")

    val signed = "{\n  \"message\": \"test\",\n  \"publickey\": \"sZZB5hoNiKfQwyTh2xNTBH87a9FraRGgnjTcCrmu5qa\",\n  \"signature\": \"3cVSpApm5PfqRMxP4a5dw3KYjBorY7316kD4DBjur52r6M7cDjGY53VMtjWLTcqf8e9pr7zAFo2j9mF8eqtRUAvh\"\n}"
    (POST.request(s"/addresses/verifyText/$address", body = signed) \ "valid").as[Boolean] shouldBe true

    val incorrect = "{\n  \"message\": \"test2\",\n  \"publickey\": \"sZZB5hoNiKfQwyTh2xNTBH87a9FraRGgnjTcCrmu5qa\",\n  \"signature\": \"3cVSpApm5PfqRMxP4a5dw3KYjBorY7316kD4DBjur52r6M7cDjGY53VMtjWLTcqf8e9pr7zAFo2j9mF8eqtRUAvh\"\n}"
    (POST.request(s"/addresses/verifyText/$address", body = incorrect) \ "valid").as[Boolean] shouldBe false
  }

  test("POST /addresses/signText/{address} API route") {
    val message = "test"
    val req = POST.request(s"/addresses/signText/$address", body = message)
    (req \ "message").as[String] shouldBe message
    val pubkey = (req \ "publickey").asOpt[String].flatMap(Base58.decode(_).toOption)
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

  test("/addresses/ API route") {
    val response = GET.request("/addresses")
    response.as[List[String]] shouldBe addresses
  }

  test("POST /addresses/verify/{address} API route") {
    val address = "jACSbUoHi4eWgNu6vzAnEx583NwmUAVfS"
    POST.incorrectApiKeyTest(s"/addresses/verify/$address")

    val signed = "{\n  \"message\": \"3yZe7d\",\n  \"publickey\": \"sZZB5hoNiKfQwyTh2xNTBH87a9FraRGgnjTcCrmu5qa\",\n  \"signature\": \"5Tt2JiPh3F17sTckvBg9GooHKjuFAFyNVXz9epDwrLWZShah4xV5cjXvUeQvbx8R545LmucdnZdPfLeqDkL3PijJ\"\n}"
    (POST.request(s"/addresses/verify/$address", body = signed) \ "valid").as[Boolean] shouldBe true

    val incorrect = "{\n  \"message\": \"3yZea7d\",\n  \"publickey\": \"sZZB5hoNiKfQwyTh2xNTBH87a9FraRGgnjTcCrmu5qa\",\n  \"signature\": \"5Tt2JiPh3F17sTckvBg9GooHKjuFAFyNVXz9epDwrLWZShah4xV5cjXvUeQvbx8R545LmucdnZdPfLeqDkL3PijJ\"\n}"
    (POST.request(s"/addresses/verify/$address", body = incorrect) \ "valid").as[Boolean] shouldBe false
  }

  def accounts = wallet.privateKeyAccounts()

  def addresses = accounts.map(_.address)

}