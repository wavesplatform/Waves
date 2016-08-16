package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}
import scorex.crypto.encode.Base58
import scorex.lagonaki.integration.TestLock

class WalletAPISpecification extends FunSuite with TestLock with Matchers {

  import scorex.lagonaki.TestingCommons._

  test("/wallet/ API route") {
    (GET.request("/wallet") \ "exists").as[Boolean] shouldBe true
  }

  test("/wallet/seed API route") {
    GET.incorrectApiKeyTest("/wallet/seed")

    val response = GET.request("/wallet/seed", headers =  Map("api_key" -> "test"))
    (response \ "seed").as[String] shouldBe Base58.encode(application.settings.walletSeed.get)
  }
}