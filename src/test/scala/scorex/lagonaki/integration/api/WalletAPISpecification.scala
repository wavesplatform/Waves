package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}
import scorex.crypto.encode.Base58

class WalletAPISpecification extends FunSuite with Matchers with scorex.waves.TestingCommons {

  override def beforeAll(): Unit = {
    super.beforeAll()
    stopGeneration(applications)
  }

  test("/wallet/seed API route") {
    GET.incorrectApiKeyTest("/wallet/seed")

    val response = GET.request("/wallet/seed", headers = Map("api_key" -> "test"))
    (response \ "seed").as[String] shouldBe Base58.encode(application.settings.walletSeed.get)
  }
}
