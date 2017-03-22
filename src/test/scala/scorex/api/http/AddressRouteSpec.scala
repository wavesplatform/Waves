package scorex.api.http

import akka.http.scaladsl.model.headers.RawHeader
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.PathMockFactory
import scorex.transaction.State
import scorex.wallet.Wallet
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.RestAPISettings
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.JsObject
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash

class AddressRouteSpec extends RouteSpec("/addresses/") with PathMockFactory with PropertyChecks {
  private val apiKey = "test_key"

  private val settings = {
    val keyHash = Base58.encode(SecureCryptographicHash(apiKey))
    RestAPISettings.fromConfig(
      ConfigFactory
        .parseString(s"waves.rest-api.api-key-hash = $keyHash")
        .withFallback(ConfigFactory.load()))
  }

  private val wallet = {
    val file = scorex.createTestTemporaryFile("wallet", ".dat")
    val wallet = new Wallet(Some(file.getCanonicalPath), "123", None)
    wallet.generateNewAccounts(10)
    wallet
  }

  private val allAccounts = wallet.privateKeyAccounts()
  private val allAddresses = allAccounts.map(_.address)

  private val state = {
    val m = mock[State]
    (m.balance _).expects(*).returning(0L).anyNumberOfTimes()
    (m.balanceWithConfirmations _).expects(*, *).returning(0L).anyNumberOfTimes()
    m
  }

  private val route = AddressApiRoute(settings, wallet, state).route

  routePath("seq/{from}/{to}") in {
    val r1 = Get(routePath("seq/1/4")) ~> route ~> check {
      val response = responseAs[Seq[String]]
      response.length shouldBe 3
      allAddresses should contain allElementsOf response
      response
    }

    val r2 = Get(routePath("seq/5/9")) ~> route ~> check {
      val response = responseAs[Seq[String]]
      response.length shouldBe 4
      allAddresses should contain allElementsOf response
      response
    }

    r1 shouldNot contain allElementsOf r2
  }

  routePath("validate/{address}") in {
    val t = Table(("address", "valid"),
      allAddresses.map(_ -> true) :+ "invalid-address" -> false: _*
    )

    forAll(t) { (a, v) =>
      Get(routePath(s"validate/$a")) ~> route ~> check {
        val r = responseAs[AddressApiRoute.Validity]
        r.address shouldEqual a
        r.valid shouldBe v
      }
    }
  }

  routePath("seed/{address}") in {
    val account = allAccounts.head
    val path = routePath(s"seed/${account.address}")
    Get(path) ~> route should produce(ApiKeyNotValid)
    Get(path).addHeader(RawHeader("api_key", apiKey)) ~> route ~> check {
      val json = responseAs[JsObject]
      (json \ "address").as[String] shouldEqual account.address
      (json \ "seed").as[String] shouldEqual Base58.encode(account.seed)
    }
  }

  routePath("balance/{address}") in {
    Get(routePath(s"balance/${allAddresses.head}")) ~> route ~> check {
      val r = responseAs[AddressApiRoute.Balance]
      r.balance shouldEqual 0
      r.confirmations shouldEqual 0
    }
  }

  routePath("sign/{address}") in {

  }

  routePath("balance/{address}/{confirmations}") in {
//    def noShrink[A]: Shrink[A] = Shrink[A](_ => Stream.empty)
//    implicit val str = noShrink[String]
//    implicit val int = noShrink[Int]
//    implicit val long = noShrink[Long]

    val gen = for {
      address <- Gen.oneOf(allAddresses).label("address")
      confirmations <- Gen.choose(1, Int.MaxValue).label("confirmations")
      balances <- Gen.choose(1L, Long.MaxValue).label("balance")
    } yield (address, confirmations, balances)

    forAll(gen) {
      case (address, confirmations, balances) =>
        allAddresses should contain (address)

//        println(s"$address:$confirmations:$balances")
        val m = mock[State]
        (m.balanceWithConfirmations _).expects(*, confirmations).returning(balances).once()
        val r = AddressApiRoute(settings, wallet, m).route

        Get(routePath(s"balance/$address/$confirmations")) ~> r ~> check {
          val b = responseAs[AddressApiRoute.Balance]
          b.balance shouldEqual balances
          b.confirmations shouldEqual confirmations
        }
    }
  }

  routePath("{address}") in {

  }

  routePath("verifyText/{address}") in pending

  routePath("signText/{address}") in pending

  routePath("") in {

  }

  routePath("verify/{address}") in pending

  routePath("publicKey/{publicKey}") in pending
}
