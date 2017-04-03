package com.wavesplatform.http

import com.typesafe.config.ConfigFactory
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.Portfolio
import com.wavesplatform.state2.reader.StateReader
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import scorex.api.http.{AddressApiRoute, ApiKeyNotValid, InvalidMessage}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash
import scorex.transaction.State
import scorex.wallet.Wallet

class AddressRouteSpec extends RouteSpec("/addresses") with PathMockFactory with PropertyChecks with RestAPISettingsHelper {
  import org.scalacheck.Shrink

  implicit val noShrink: Shrink[String] = Shrink.shrinkAny

  private val wallet = {
    val file = scorex.createTestTemporaryFile("wallet", ".dat")
    val wallet = new Wallet(Some(file.getCanonicalPath), "123", None)
    wallet.generateNewAccounts(10)
    wallet
  }

  private val allAccounts = wallet.privateKeyAccounts()
  private val allAddresses = allAccounts.map(_.address)

  private val state = {
    val m = mock[StateReader]
    (m.accountPortfolio _).expects(*).returning(Portfolio(0L, 0L, Map.empty)).anyNumberOfTimes()
    m
  }

  private val route = AddressApiRoute(restAPISettings, wallet, state).route

  private val generatedMessages = for {
    account <- Gen.oneOf(allAccounts).label("account")
    message <- Gen.alphaNumStr.label("message").suchThat(_.length > 10)
  } yield (account, message)

  routePath("/seq/{from}/{to}") in {
    val r1 = Get(routePath("/seq/1/4")) ~> route ~> check {
      val response = responseAs[Seq[String]]
      response.length shouldBe 3
      allAddresses should contain allElementsOf response
      response
    }

    val r2 = Get(routePath("/seq/5/9")) ~> route ~> check {
      val response = responseAs[Seq[String]]
      response.length shouldBe 4
      allAddresses should contain allElementsOf response
      response
    }

    r1 shouldNot contain allElementsOf r2
  }

  routePath("/validate/{address}") in {
    val t = Table(("address", "valid"),
      allAddresses.map(_ -> true) :+ "invalid-address" -> false: _*
    )

    forAll(t) { (a, v) =>
      Get(routePath(s"/validate/$a")) ~> route ~> check {
        val r = responseAs[AddressApiRoute.Validity]
        r.address shouldEqual a
        r.valid shouldBe v
      }
    }
  }

  routePath("/seed/{address}") in {
    val account = allAccounts.head
    val path = routePath(s"/seed/${account.address}")
    Get(path) ~> route should produce(ApiKeyNotValid)
    Get(path) ~> api_key(apiKey) ~> route ~> check {
      val json = responseAs[JsObject]
      (json \ "address").as[String] shouldEqual account.address
      (json \ "seed").as[String] shouldEqual Base58.encode(account.seed)
    }
  }

  routePath("/balance/{address}") in {
    Get(routePath(s"/balance/${allAddresses.head}")) ~> route ~> check {
      val r = responseAs[AddressApiRoute.Balance]
      r.balance shouldEqual 0
      r.confirmations shouldEqual 0
    }
  }

  private def testSign(path: String, encode: Boolean): Unit =
    forAll(generatedMessages) { case (account, message) =>
      val uri = routePath(s"/$path/${account.address}")
      Post(uri, message) ~> route should produce(ApiKeyNotValid)
      Post(uri, message) ~> api_key(apiKey) ~> route ~> check {
        val resp = responseAs[JsObject]
        val signature = Base58.decode((resp \ "signature").as[String]).get

        (resp \ "message").as[String] shouldEqual (if (encode) Base58.encode(message.getBytes) else message)
        (resp \ "publicKey").as[String] shouldEqual Base58.encode(account.publicKey)

        EllipticCurveImpl.verify(signature, message.getBytes, account.publicKey) shouldBe true
      }
    }

  routePath("/sign/{address}") in testSign("sign", true)
  routePath("/signText/{address}") in testSign("signText", false)

  private def testVerify(path: String, encode: Boolean): Unit = {

    forAll(generatedMessages) { case (account, message) =>
      val uri = routePath(s"/$path/${account.address}")
      val messageBytes = message.getBytes()
      val signature = EllipticCurveImpl.sign(account, messageBytes)
      val validBody = Json.obj("message" -> JsString(if (encode) Base58.encode(messageBytes) else message),
        "publickey" -> JsString(Base58.encode(account.publicKey)),
        "signature" -> JsString(Base58.encode(signature)))

      val emptySignature = Json.obj("message" -> JsString(""),
        "publickey" -> JsString(Base58.encode(account.publicKey)),
        "signature" -> JsString(""))

      Post(uri, validBody) ~> route should produce(ApiKeyNotValid)
      Post(uri, emptySignature) ~> api_key(apiKey) ~> route should produce(InvalidMessage)
      Post(uri, validBody) ~> api_key(apiKey) ~> route ~> check {
        (responseAs[JsObject] \ "valid").as[Boolean] shouldBe true
      }
    }
  }

  routePath("/verifyText/{address}") in testVerify("verifyText", false)
  routePath("/verify/{address}") in testVerify("verify", true)

  routePath("/balance/{address}/{confirmations}") in {
    val gen = for {
      address <- Gen.oneOf(allAddresses).label("address")
      confirmations <- Gen.choose(1, Int.MaxValue).label("confirmations")
      balances <- Gen.choose(1L, Long.MaxValue).label("balance")
    } yield (address, confirmations, balances)

    forAll(gen) {
      case (address, confirmations, balances) =>
        allAddresses should contain(address)

        val m = mock[StateReader]
        //        (m.balanceWithConfirmations _).expects(*, confirmations).returning(balances).once()
        ???
        val r = AddressApiRoute(restAPISettings, wallet, m).route

        Get(routePath(s"/balance/$address/$confirmations")) ~> r ~> check {
          val b = responseAs[AddressApiRoute.Balance]
          b.balance shouldEqual balances
          b.confirmations shouldEqual confirmations
        }
    }
  }

  routePath("") in {
    Post(routePath("")) ~> route should produce(ApiKeyNotValid)
    Post(routePath("")) ~> api_key(apiKey) ~> route ~> check {
      allAddresses should not contain (responseAs[JsObject] \ "address").as[String]
    }
  }

  routePath("/{address}") in {
    Delete(routePath(s"/${allAddresses.head}")) ~> api_key(apiKey) ~> route ~> check {
      (responseAs[JsObject] \ "deleted").as[Boolean] shouldBe true
    }
  }

  routePath("/publicKey/{publicKey}") in pending
}
