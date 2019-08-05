package com.wavesplatform.http

// [WAIT] import cats.kernel.Monoid
import java.net.{URLDecoder, URLEncoder}

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.api.http.AddressApiRoute
import com.wavesplatform.api.http.ApiError.ApiKeyNotValid
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import com.wavesplatform.state.StringDataEntry

import scala.concurrent.Future
import scala.util.Random
// [WAIT] import com.wavesplatform.lang.{Global, StdLibVersion}
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{VerifierAnnotation, VerifierFunction}
// [WAIT] import com.wavesplatform.lang.v1.compiler.Decompiler
import com.wavesplatform.lang.v1.compiler.Terms._
// [WAIT] import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.{NoShrink, TestTime, TestWallet, crypto}
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

  private val allAccounts               = testWallet.privateKeyAccounts
  private val allAddresses              = allAccounts.map(_.stringRepr)
  private val blockchain                = stub[Blockchain]
  private[this] val utxPoolSynchronizer = stub[UtxPoolSynchronizer]
  (utxPoolSynchronizer.publishTransaction _).when(*, *, *).returns(Future.successful(Right(true)))

  private val route =
    AddressApiRoute(restAPISettings, testWallet, blockchain, utxPoolSynchronizer, new TestTime).route

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
    val path    = routePath(s"/seed/${account.stringRepr}")
    Get(path) ~> route should produce(ApiKeyNotValid)
    Get(path) ~> ApiKeyHeader ~> route ~> check {
      val json = responseAs[JsObject]
      (json \ "address").as[String] shouldEqual account.stringRepr
      (json \ "seed").as[String] shouldEqual Base58.encode(account.seed)
    }
  }

  private def testSign(path: String, encode: Boolean): Unit =
    forAll(generatedMessages) {
      case (account, message) =>
        val uri = routePath(s"/$path/${account.stringRepr}")
        Post(uri, message) ~> route should produce(ApiKeyNotValid)
        Post(uri, message) ~> ApiKeyHeader ~> route ~> check {
          val resp      = responseAs[JsObject]
          val signature = Base58.tryDecodeWithLimit((resp \ "signature").as[String]).get

          (resp \ "message").as[String] shouldEqual (if (encode) Base58.encode(message.getBytes("UTF-8")) else message)
          (resp \ "publicKey").as[String] shouldEqual Base58.encode(account.publicKey)

          crypto.verify(signature, message.getBytes("UTF-8"), account.publicKey) shouldBe true
        }
    }

  routePath("/sign/{address}") in testSign("sign", true)
  routePath("/signText/{address}") in testSign("signText", false)

  private def testVerify(path: String, encode: Boolean): Unit = {

    forAll(generatedMessages.flatMap(m => Gen.oneOf(true, false).map(b => (m, b)))) {
      case ((account, message), b58) =>
        val uri          = routePath(s"/$path/${account.stringRepr}")
        val messageBytes = message.getBytes("UTF-8")
        val signature    = crypto.sign(account, messageBytes)
        val validBody = Json.obj(
          "message"   -> JsString(if (encode) if (b58) Base58.encode(messageBytes) else "base64:" ++ Base64.encode(messageBytes) else message),
          "publickey" -> JsString(Base58.encode(account.publicKey)),
          "signature" -> JsString(Base58.encode(signature))
        )

        val emptySignature =
          Json.obj("message" -> JsString(""), "publickey" -> JsString(Base58.encode(account.publicKey)), "signature" -> JsString(""))

        Post(uri, validBody) ~> route should produce(ApiKeyNotValid)
        Post(uri, emptySignature) ~> ApiKeyHeader ~> route ~> check {
          (responseAs[JsObject] \ "valid").as[Boolean] shouldBe false
        }
        Post(uri, validBody) ~> ApiKeyHeader ~> route ~> check {
          (responseAs[JsObject] \ "valid").as[Boolean] shouldBe true
        }
    }
  }

  routePath("/verifyText/{address}") in testVerify("verifyText", false)
  routePath("/verify/{address}") in testVerify("verify", true)

  routePath("") in {
    Post(routePath("")) ~> route should produce(ApiKeyNotValid)
    Post(routePath("")) ~> ApiKeyHeader ~> route ~> check {
      allAddresses should not contain (responseAs[JsObject] \ "address").as[String]
    }
  }

  routePath("/{address}") in {
    Delete(routePath(s"/${allAddresses.head}")) ~> ApiKeyHeader ~> route ~> check {
      (responseAs[JsObject] \ "deleted").as[Boolean] shouldBe true
    }
  }

  routePath(s"/scriptInfo/${allAddresses(1)}") in {
    (blockchain.accountScript _).when(allAccounts(1).toAddress).onCall((_: AddressOrAlias) => Some(ExprScript(TRUE).explicitGet()))
    Get(routePath(s"/scriptInfo/${allAddresses(1)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(1)
      (response \ "script").as[String] shouldBe "base64:AQa3b8tH"
      (response \ "scriptText").as[String] shouldBe "true"
      (response \ "complexity").as[Long] shouldBe 1
      (response \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    (blockchain.accountScript _).when(allAccounts(2).toAddress).onCall((_: AddressOrAlias) => None)
    Get(routePath(s"/scriptInfo/${allAddresses(2)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(2)
      (response \ "script").asOpt[String] shouldBe None
      (response \ "scriptText").asOpt[String] shouldBe None
      (response \ "complexity").as[Long] shouldBe 0
      (response \ "extraFee").as[Long] shouldBe 0
    }

    val contractWithMeta = DApp(
      meta = DAppMeta(
        version = 1,
        List(
          CallableFuncSignature(ByteString.copyFrom(Array[Byte](1, 2, 3)))
        )
      ),
      decs = List(),
      callableFuncs = List(),
      verifierFuncOpt = Some(VerifierFunction(VerifierAnnotation("t"), FUNC("verify", List(), TRUE)))
    )
    (blockchain.accountScript _)
      .when(allAccounts(3).toAddress)
      .onCall((_: AddressOrAlias) => Some(ContractScript(V3, contractWithMeta).explicitGet()))
    Get(routePath(s"/scriptInfo/${allAddresses(3)}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "address").as[String] shouldBe allAddresses(3)
      // [WAIT] (response \ "script").as[String] shouldBe "base64:AAIDAAAAAAAAAA[QBAgMEAAAAAAAAAAAAAAABAAAAAXQBAAAABnZlcmlmeQAAAAAG65AUYw=="
      (response \ "script").as[String] shouldBe "base64:AAIDAAAAAAAAAAkIARIFCgMBAgMAAAAAAAAAAAAAAAEAAAABdAEAAAAGdmVyaWZ5AAAAAAYSVyVy"
      (response \ "scriptText").as[String] should fullyMatch regex ("DApp\\(" +
        "DAppMeta\\(" +
        "1," +
        "List\\(CallableFuncSignature\\(<ByteString@(.*) size=3>\\)\\)\\)," +
        "List\\(\\)," +
        "List\\(\\)," +
        "Some\\(VerifierFunction\\(VerifierAnnotation\\(t\\),FUNC\\(verify,List\\(\\),true\\)\\)\\)" +
      "\\)").r
      // [WAIT]                                           Decompiler(
      //      testContract,
      //      Monoid.combineAll(Seq(PureContext.build(com.wavesplatform.lang.directives.values.StdLibVersion.V3), CryptoContext.build(Global))).decompilerContext)
      (response \ "complexity").as[Long] shouldBe 11
      (response \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }
  }

  routePath(s"/data/${allAddresses(1)}?matches=regex") in {
    val dataKeys = List(
      "abc",
      "aBcD",
      "ABD",
      "ac",
      "ab1c",
      "1aB1cD",
      "A1BD0",
      "a110b",
      "123",
      "reeee",
      " ab",
      "ab ",
      "a b\n",
      "ab1 \n\t",
      "\n\raB1\t",
      "\n  \r  \t\t",
      "!#$%&'()*+,",
      "!#$%&'()<*=>+,",
      "!",
      "<!@#qwe>",
      "\\",
      "\"\\",
      "qwe!",
      "\b",
      "\u0000",
      "\b\b",
      "\bqweasd\u0000",
      "\u0000qweqwe"
    )

    val testData: Map[String, String] =
      dataKeys.map { k =>
        k -> Random.nextString(16)
      }.toMap

    val regexps = List(
      /*"abc", "bca",*/
      "[a-zA-Z]{1,}",
      "[a-z0-9]{0,4}",
      "[a-zA-Z0-9]{1,4}",
      "[a-z!-/]{2,}",
      "[!-/:-@]{0,}",
      "re*",
      "re.ee",
      "\\w{0,}",
      "\\d{0,}",
      "[\\w\\d]{0,}",
      "\\s{0,}",
      "^1aB1cD$",
      "(a|b)c"
    )

    (blockchain.accountDataKeys _)
      .when(allAccounts(1).toAddress)
      .returning(dataKeys)
      .anyNumberOfTimes()

    testData.foreach {
      case (k, v) =>
        (blockchain
          .accountData(_: Address, _: String))
          .when(allAccounts(1).toAddress, k)
          .returning(Some(StringDataEntry(k, v)))
          .anyNumberOfTimes()
    }

    for (regex <- regexps) {
      Get(routePath(s"""/data/${allAddresses(1)}?matches=$regex""")) ~> route ~> check {
        val kvs = responseAs[JsArray].value
          .map { json =>
            ((json \ "key").as[String], (json \ "value").as[String])
          }
          .toList
          .sortBy(_._1)

        val regexPattern = regex.r.pattern
        kvs shouldEqual testData.filter(k => regexPattern.matcher(k._1).matches()).toSeq.sortBy(_._1)
      }
    }

    for (regex <- regexps.map(rgx => URLEncoder.encode(rgx, "UTF-8"))) {
      Get(routePath(s"""/data/${allAddresses(1)}?matches=$regex""")) ~> route ~> check {
        val kvs = responseAs[JsArray].value
          .map { json =>
            ((json \ "key").as[String], (json \ "value").as[String])
          }
          .toList
          .sortBy(_._1)

        val regexPattern = URLDecoder.decode(regex, "UTF-8").r.pattern
        kvs shouldEqual testData.filter(k => regexPattern.matcher(k._1).matches()).toSeq.sortBy(_._1)
      }
    }

    val invalidRegexps = List("[a-z", "([a-z]{0}", "[a-z]{0", "[a-z]{,5}")
    for (regex <- invalidRegexps) {
      Get(routePath(s"""/data/${allAddresses(1)}?matches=$regex""")) ~> route ~> check {
        responseAs[String] should include("Cannot compile regex")
      }
    }
  }
}
