package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.JsValue
import scorex.crypto.encode.Base58
import scorex.lagonaki.TransactionTestingCommons
import scorex.transaction.BlockChain


class BlockAPISpecification extends FunSuite with Matchers with TransactionTestingCommons {

  private val history = application.blockStorage.history
  private val genesis = history.genesis

  override def beforeAll(): Unit = {
    super.beforeAll()
    stopGeneration(applications)

    while (history.height() < 3) {
      application.blockStorage.appendBlock(genValidBlock())
    }

    startGeneration(applications)
  }

  private def last = history.lastBlock

  test("GET /blocks/at/{height} API route") {
    val response = GET.request(s"/blocks/at/1")
    checkGenesis(response)
    (response \ "height").as[Int] shouldBe 1
  }

  test("GET /blocks/seq/{from}/{to} API route") {
    val response = GET.request(s"/blocks/seq/1/3")
    checkGenesis(response(0).as[JsValue])
    checkBlock(response(1).as[JsValue])
    (response(1) \ "height").as[Int] shouldBe 2
    (response(2) \ "height").as[Int] shouldBe 3
  }

  test("GET /blocks/last API route") {
    val response = GET.request(s"/blocks/last")
    checkBlock(response)
  }

  test("GET /blocks/height API route") {
    val response = GET.request(s"/blocks/height")
    (response \ "height").as[Int] shouldBe history.height()
  }

  test("GET /blocks/child/{signature} API route") {
    val response = GET.request(s"/blocks/child/${genesis.encodedId}")
    checkBlock(response)
    (response \ "signature").as[String] shouldBe history.asInstanceOf[BlockChain].blockAt(2).get.encodedId
  }

  test("GET /blocks/delay/{signature}/{blockNum} API route") {
    val response = GET.request(s"/blocks/delay/${last.encodedId}/1")
    (response \ "delay").as[Long] should be > 0L
  }

  test("GET /blocks/height/{signature} API route") {
    val response = GET.request(s"/blocks/height/${genesis.encodedId}")
    (response \ "height").as[Int] shouldBe 1
  }

  test("GET /blocks/signature/{signature} API route") {
    Base58.decode(genesis.encodedId).toOption.map(signature => history.blockById(signature)).isDefined shouldBe true
    checkGenesis(GET.request(s"/blocks/signature/${genesis.encodedId}"))
    val response = GET.request(s"/blocks/signature/${last.encodedId}")
    checkBlock(response)
    (response \ "height").as[Int] shouldBe history.heightOf(last).get
  }

  test("GET /blocks/first API route") {
    val response = GET.request(s"/blocks/first")
    checkGenesis(response)
    (response \ "height").as[Int] shouldBe 1
  }

  test("GET /blocks/address/{address}/{from}/{to} API route") {
    checkGenesis(GET.request(s"/blocks/address/3Mp6FarByk73bgv3CFnbrzMzWgLmMHAJnj2/0/1")(0).as[JsValue])
  }


  def checkGenesis(response: JsValue): Unit = {
    (response \ "reference").as[String] shouldBe "67rpwLCuS5DGA8KGZXKsVQ7dnPb9goRLoKfgGbLfQg9WoLUgNY77E2jT11fem3coV9nAkguBACzrU1iyZM4B8roQ"
    (response \ "transactions" \\ "fee").toList.size shouldBe 5
    (response \ "generator").as[String] shouldBe "3Mp6FarByk73bgv3CFnbrzMzWgLmMHAJnj2"
    (response \ "signature").as[String] shouldBe Base58.encode(genesis.uniqueId)
    (response \ "fee").as[Int] shouldBe 0
    checkBlock(response)
  }

  def checkBlock(response: JsValue): Unit = {
    (response \ "version").asOpt[Int].isDefined shouldBe true
    (response \ "timestamp").as[Long] should be >= 0L
    (response \ "reference").asOpt[String].isDefined shouldBe true
    (response \ "transactions" \\ "fee").toList.size should be >= 0
    (response \ "generator").asOpt[String].isDefined shouldBe true
    (response \ "signature").asOpt[String].isDefined shouldBe true
    (response \ "fee").as[Int] should be >= 0
    (response \ "blocksize").as[Int] should be > 0
  }

}
