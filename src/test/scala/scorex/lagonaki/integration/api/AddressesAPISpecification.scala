package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}
import scorex.crypto.encode.Base58
import scorex.lagonaki.TestingCommons


class AddressesAPISpecification extends FunSuite with Matchers {

  import TestingCommons._

  val wallet = application.wallet
  if (wallet.privateKeyAccounts().size < 10) wallet.generateNewAccounts(10)
  val accounts = wallet.privateKeyAccounts()
  val addresses = accounts.map(_.address)
  val account = accounts.head
  val address = account.address


  test("/addresses/seq/{from}/{to} API route") {
    val responsed = getRequest("/addresses/seq/1/4").as[List[String]]
    responsed.size shouldBe 3
    responsed.foreach(a => addresses should contain(a))

    val r2 = getRequest("/addresses/seq/5/9").as[List[String]]
    r2.size shouldBe 4
    r2.foreach(a => addresses should contain(a))
    responsed.intersect(r2).isEmpty shouldBe true

  }

  test("/addresses/validate/{address} API route") {
    val toCheck: Seq[(String, Boolean)] = ("wrongA", false) +: addresses.map(a => (a, true))
    toCheck.foreach { a =>
      val response = getRequest(s"/addresses/validate/${a._1}")
      (response \ "address").as[String] shouldBe a._1
      (response \ "valid").as[Boolean] shouldBe a._2
    }
  }

  test("/addresses/seed/{address} API route") {
    val response = getRequest(s"/addresses/seed/${account.address}")
    (response \ "address").as[String] shouldBe account.address
    (response \ "seed").as[String] shouldBe Base58.encode(account.seed)
  }

  test("/addresses/balance/{address} API route") {
    val response = getRequest(s"/addresses/balance/$address")
    (response \ "address").as[String] shouldBe address
    (response \ "confirmations").as[Int] shouldBe 1
    (response \ "balance").as[Long] should be >= 0L
  }

  test("POST /addresses/sign/{address} API route") {
    //TODO
  }

  test("/addresses/balance/{address}/{confirmations} API route") {
    val confirmations = Math.min(3, application.blockStorage.state.stateHeight)
    val response = getRequest(s"/addresses/balance/$address/$confirmations")
    (response \ "address").as[String] shouldBe address
    (response \ "confirmations").as[Int] shouldBe confirmations
    (response \ "balance").as[Long] should be >= 0L
  }

  test("/addresses/generatingbalance/{address} API route") {
    val response = getRequest(s"/addresses/generatingbalance/$address")
    (response \ "address").as[String] shouldBe address
    (response \ "balance").as[Long] should be >= 0L
  }

  test("DELETE /addresses/{address} API route") {
    //TODO
  }

  test("POST /addresses/verifyText/{address} API route") {
    //TODO
  }

  test("POST /addresses/signText/{address} API route") {
    //TODO
  }

  test("POST /addresses API route") {
    //TODO
  }

  test("/addresses/ API route") {
    val response = getRequest("/addresses")
    response.as[List[String]] shouldBe addresses
  }

  test("POST /addresses/verify/{address} API route") {
    //TODO
  }


}