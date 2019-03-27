package com.wavesplatform.http

// [WAIT] import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.{AddressApiRoute, ApiKeyNotValid}
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.http.ApiMarshallers._
// [WAIT] import com.wavesplatform.lang.{Global, StdLibVersion}
import com.wavesplatform.lang.StdLibVersion
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{VerifierAnnotation, VerifierFunction}
// [WAIT] import com.wavesplatform.lang.v1.compiler.Decompiler
import com.wavesplatform.lang.v1.compiler.Terms._
// [WAIT] import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.transaction.smart.script.ContractScript
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.{NoShrink, TestTime, TestWallet, crypto}
import io.netty.channel.group.ChannelGroup
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json._

class AddressRouteSpec
    extends RouteSpec("/addresses")
    with PathMockFactory
    with PropertyChecks
    with RestAPISettingsHelper
    with TestWallet
    with NoShrink {

  private val allAccounts  = testWallet.privateKeyAccounts
  private val allAddresses = allAccounts.map(_.address)
  private val blockchain   = stub[Blockchain]

  private val route = AddressApiRoute(
    restAPISettings,
    testWallet,
    blockchain,
    mock[UtxPool],
    mock[ChannelGroup],
    new TestTime,
    TestFunctionalitySettings.Stub
  ).route

  private val generatedMessages = for {
    account <- Gen.oneOf(allAccounts).label("account")
    length  <- Gen.chooseNum(10, 1000)
    message <- Gen.listOfN(length, Gen.alphaNumChar).map(_.mkString).label("message")
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
    val t = Table(("address", "valid"), allAddresses.map(_ -> true) :+ "invalid-address" -> false: _*)

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
    val path    = routePath(s"/seed/${account.address}")
    Get(path) ~> route should produce(ApiKeyNotValid)
    Get(path) ~> api_key(apiKey) ~> route ~> check {
      val json = responseAs[JsObject]
      (json \ "address").as[String] shouldEqual account.address
      (json \ "seed").as[String] shouldEqual Base58.encode(account.seed)
    }
  }

  private def testSign(path: String, encode: Boolean): Unit =
    forAll(generatedMessages) {
      case (account, message) =>
        val uri = routePath(s"/$path/${account.address}")
        Post(uri, message) ~> route should produce(ApiKeyNotValid)
        Post(uri, message) ~> api_key(apiKey) ~> route ~> check {
          val resp      = responseAs[JsObject]
          val signature = Base58.tryDecodeWithLimit((resp \ "signature").as[String]).get

          (resp \ "message").as[String] shouldEqual (if (encode) Base58.encode(message.getBytes) else message)
          (resp \ "publicKey").as[String] shouldEqual Base58.encode(account.publicKey)

          crypto.verify(signature, message.getBytes, account.publicKey) shouldBe true
        }
    }

  routePath("/sign/{address}") in testSign("sign", true)
  routePath("/signText/{address}") in testSign("signText", false)

  private def testVerify(path: String, encode: Boolean): Unit = {

    forAll(generatedMessages.flatMap(m => Gen.oneOf(true, false).map(b => (m, b)))) {
      case ((account, message), b58) =>
        val uri          = routePath(s"/$path/${account.address}")
        val messageBytes = message.getBytes()
        val signature    = crypto.sign(account, messageBytes)
        val validBody = Json.obj(
          "message"   -> JsString(if (encode) if (b58) Base58.encode(messageBytes) else ("base64:" ++ Base64.encode(messageBytes)) else message),
          "publickey" -> JsString(Base58.encode(account.publicKey)),
          "signature" -> JsString(Base58.encode(signature))
        )

        val emptySignature =
          Json.obj("message" -> JsString(""), "publickey" -> JsString(Base58.encode(account.publicKey)), "signature" -> JsString(""))

        Post(uri, validBody) ~> route should produce(ApiKeyNotValid)
        Post(uri, emptySignature) ~> api_key(apiKey) ~> route ~> check {
          (responseAs[JsObject] \ "valid").as[Boolean] shouldBe false
        }
        Post(uri, validBody) ~> api_key(apiKey) ~> route ~> check {
          (responseAs[JsObject] \ "valid").as[Boolean] shouldBe true
        }
    }
  }

  routePath("/verifyText/{address}") in testVerify("verifyText", false)
  routePath("/verify/{address}") in testVerify("verify", true)

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

  routePath(s"/scriptInfo/${allAddresses(1)}") in {
    (blockchain.accountScript _).when(allAccounts(1).toAddress).onCall((_: Address) => Some(ExprScript(TRUE).explicitGet()))
    Get(routePath(s"/scriptInfo/${allAddresses(1)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(1)
      (response \ "script").as[String] shouldBe "base64:AQa3b8tH"
      (response \ "scriptText").as[String] shouldBe "TRUE" // [WAIT] "true"
      (response \ "complexity").as[Long] shouldBe 1
      (response \ "extraFee").as[Long] shouldBe CommonValidation.ScriptExtraFee
    }

    (blockchain.accountScript _).when(allAccounts(2).toAddress).onCall((_: Address) => None)
    Get(routePath(s"/scriptInfo/${allAddresses(2)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(2)
      (response \ "script").asOpt[String] shouldBe None
      (response \ "scriptText").asOpt[String] shouldBe None
      (response \ "complexity").as[Long] shouldBe 0
      (response \ "extraFee").as[Long] shouldBe 0
    }

    val testContract = DApp(List(), List(), Some(VerifierFunction(VerifierAnnotation("t"), FUNC("verify", List(), TRUE))))
    (blockchain.accountScript _)
      .when(allAccounts(3).toAddress)
      .onCall((_: Address) => Some(ContractScript(StdLibVersion.V3, testContract).explicitGet()))
    Get(routePath(s"/scriptInfo/${allAddresses(3)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(3)
      // [WAIT] (response \ "script").as[String] shouldBe "base64:AAIDAAAAAAAAAAAAAAAAAAAAAQAAAAF0AAAABnZlcmlmeQAAAAAAAAABBt/lCgQ="
      (response \ "script").as[String] shouldBe "base64:AAIDAAAAAAAAAAAAAAAAAAAAAQAAAAF0AQAAAAZ2ZXJpZnkAAAAABiDCPeI="
      (response \ "scriptText").as[String] shouldBe "DApp(List(),List(),Some(VerifierFunction(VerifierAnnotation(t),FUNC(verify,List(),TRUE))))"
// [WAIT]                                           Decompiler(
//      testContract,
//      Monoid.combineAll(Seq(PureContext.build(com.wavesplatform.lang.StdLibVersion.V3), CryptoContext.build(Global))).decompilerContext)
      (response \ "complexity").as[Long] shouldBe 11
      (response \ "extraFee").as[Long] shouldBe CommonValidation.ScriptExtraFee
    }
  }
}
