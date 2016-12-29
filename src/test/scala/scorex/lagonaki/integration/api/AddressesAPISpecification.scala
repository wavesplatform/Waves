package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.lagonaki.TestingCommons
import scorex.lagonaki.integration.TestLock

class AddressesAPISpecification extends FunSuite with TestLock with Matchers {

  import TestingCommons._

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    stopGeneration(applications)

    if (wallet.privateKeyAccounts().size < 10) wallet.generateNewAccounts(10)
  }

  def wallet = application.wallet
  def account = accounts.head
  def address = account.address

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
    val path =  s"/addresses/seed/${account.address}"
    GET.incorrectApiKeyTest(path)

    val response = GET.request(us = path, headers =  Map("api_key" -> "test"))
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
    val address = "3MbWTyn6Tg7zL6XbdN8TLcFMfhWX77hKcmc"
    POST.incorrectApiKeyTest(s"/addresses/verifyText/$address")

    val signed = "{\n  \"message\": \"test\",\n  \"publickey\": \"3nU4XEMkwj447BxYBRcHSp4jX2hi3Y8yHnNnfDqcT8J8\",\n  \"signature\": \"5NHde7sCZvkSbc35oaeGE5E52cZLC8p73fyYGz27urjg62e6zNB54NXaQkZgrhiKCPMgLRh5q1PSriMepSLNAkH1\"\n}"
    (POST.request(s"/addresses/verifyText/$address", body = signed) \ "valid").as[Boolean] shouldBe true

    val incorrect = "{\n  \"message\": \"test2\",\n  \"publickey\": \"3nU4XEMkwj447BxYBRcHSp4jX2hi3Y8yHnNnfDqcT8J8\",\n  \"signature\": \"5NHde7sCZvkSbc35oaeGE5E52cZLC8p73fyYGz27urjg62e6zNB54NXaQkZgrhiKCPMgLRh5q1PSriMepSLNAkH1\"\n}"
    (POST.request(s"/addresses/verifyText/$address", body = incorrect) \ "valid").as[Boolean] shouldBe false
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
    val response = GET.request("/addresses")
    response.as[List[String]] shouldBe addresses
  }

  test("POST /addresses/verify/{address} API route") {
    val address = "3MbWTyn6Tg7zL6XbdN8TLcFMfhWX77hKcmc"
    POST.incorrectApiKeyTest(s"/addresses/verify/$address")

    val signed = "{\n  \"message\": \"3yZe7d\",\n  \"publickey\": \"3nU4XEMkwj447BxYBRcHSp4jX2hi3Y8yHnNnfDqcT8J8\",\n  \"signature\": \"62nn4AZasDof2Avhk8br4ii3UTNAy4HorfeWH6W22a5HAtnqzFPTQau4HVRmrtBo5hNJJu1s5iWBNb5kE8VSKuGu\"\n}"
    (POST.request(s"/addresses/verify/$address", body = signed) \ "valid").as[Boolean] shouldBe true

    val incorrect = "{\n  \"message\": \"3yZe7dd\",\n  \"publickey\": \"3nU4XEMkwj447BxYBRcHSp4jX2hi3Y8yHnNnfDqcT8J8\",\n  \"signature\": \"62nn4AZasDof2Avhk8br4ii3UTNAy4HorfeWH6W22a5HAtnqzFPTQau4HVRmrtBo5hNJJu1s5iWBNb5kE8VSKuGu\"\n}"
    (POST.request(s"/addresses/verify/$address", body = incorrect) \ "valid").as[Boolean] shouldBe false
  }

  def accounts = wallet.privateKeyAccounts()

  def addresses = accounts.map(_.address)

}
