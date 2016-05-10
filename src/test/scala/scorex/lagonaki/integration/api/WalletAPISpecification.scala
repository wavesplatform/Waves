package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}
import scorex.crypto.encode.Base58

class WalletAPISpecification extends FunSuite with Matchers {

  import scorex.lagonaki.TestingCommons._

  test("/wallet/ API route") {
    (GET.request("/wallet") \ "exists").as[Boolean] shouldBe true
  }

  test("/wallet/seed API route") {
    (GET.request("/wallet/seed") \ "seed").as[String] shouldBe Base58.encode(application.settings.walletSeed.get)
  }

}