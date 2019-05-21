package com.wavesplatform.http

import com.wavesplatform.api.http.{TooBigArrayAllocation, UtilsApiRoute}
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{DefaultFuncAnnotation, DefaultFunction, VerifierAnnotation, VerifierFunction}
import com.wavesplatform.lang.directives.values.{V2, V3}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.utils.Time
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.{JsObject, JsValue}

class UtilsRouteSpec extends RouteSpec("/utils") with RestAPISettingsHelper with PropertyChecks {
  private val route = UtilsApiRoute(
    new Time {
      def correctedTime(): Long = System.currentTimeMillis()
      def getTimestamp(): Long  = System.currentTimeMillis()
    },
    restAPISettings
  ).route

  val script = FUNCTION_CALL(
    function = PureContext.eq.header,
    args = List(CONST_LONG(1), CONST_LONG(2))
  )

  val dappVer = DApp(
    decs = List.empty,
    callableFuncs = List.empty,
    defaultFuncOpt = None,
    verifierFuncOpt = Some(
      VerifierFunction(VerifierAnnotation("tx"), FUNC("verify", List(), TRUE))
    )
  )

  val dappDefault = DApp(
    decs = List.empty,
    callableFuncs = List.empty,
    defaultFuncOpt = Some(
      DefaultFunction(DefaultFuncAnnotation("i"), FUNC("default", List(), TRUE))
    ),
    verifierFuncOpt = None
  )

  routePath("/script/decompile") in {
    val base64 = ExprScript(script).explicitGet().bytes().base64
    Post(routePath("/script/decompile"), base64) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "STDLIB_VERSION").as[Int] shouldBe 1
      (json \ "CONTENT_TYPE").as[String] shouldBe "EXPRESSION"
      (json \ "script").as[String] shouldBe "" +
        "{-# STDLIB_VERSION 1 #-}\n" +
        "{-# CONTENT_TYPE EXPRESSION #-}\n" +
        "(1 == 2)"
    }

    //V1 Expression
    Post(routePath("/script/decompile"), "AQa3b8tH") ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "STDLIB_VERSION").as[Int] shouldBe 1
      (json \ "CONTENT_TYPE").as[String] shouldBe "EXPRESSION"
      (json \ "script").as[String] shouldBe "" +
        "{-# STDLIB_VERSION 1 #-}\n" +
        "{-# CONTENT_TYPE EXPRESSION #-}\n" +
        "true"
    }

    //V2 Expression
    Post(routePath("/script/decompile"), "AgZ7TN8j") ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "STDLIB_VERSION").as[Int] shouldBe 2
      (json \ "CONTENT_TYPE").as[String] shouldBe "EXPRESSION"
      (json \ "script").as[String] shouldBe "" +
      "{-# STDLIB_VERSION 2 #-}\n" +
      "{-# CONTENT_TYPE EXPRESSION #-}\n" +
      "true"
    }

    //V3 Expression
    Post(routePath("/script/decompile"), "AwZd0cYf") ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "STDLIB_VERSION").as[Int] shouldBe 3
      (json \ "CONTENT_TYPE").as[String] shouldBe "EXPRESSION"
      (json \ "script").as[String] shouldBe "" +
      "{-# STDLIB_VERSION 3 #-}\n" +
      "{-# CONTENT_TYPE EXPRESSION #-}\n" +
      "true"
    }

    val dappVerBytesStr = ContractScript(V3, dappVer).explicitGet().bytes().base64

    testdAppDirective(dappVerBytesStr)
    testdAppDirective("\t\t \n\n" + dappVerBytesStr + " \t \n \t")
  }

  private def testdAppDirective(str: String) =
    Post(routePath("/script/decompile"), str) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "STDLIB_VERSION").as[Int] shouldBe 3
      (json \ "CONTENT_TYPE").as[String] shouldBe "DAPP"
      (json \ "SCRIPT_TYPE").as[String] shouldBe "ACCOUNT"

      val expectedResult =
        "{-# STDLIB_VERSION 3 #-}\n{-# SCRIPT_TYPE ACCOUNT #-}\n{-# CONTENT_TYPE DAPP #-}\n\n\n\n@Verifier(tx)\nfunc verify () = true\n"
      (json \ "script").as[String] shouldBe expectedResult
    }

  routePath("/script/compile") in {
    Post(routePath("/script/compile"), "(1 == 2)") ~> route ~> check {
      val json           = responseAs[JsValue]
      val expectedScript = ExprScript(V2, script).explicitGet()

      Script.fromBase64String((json \ "script").as[String]) shouldBe Right(expectedScript)
      (json \ "complexity").as[Long] shouldBe 3
      (json \ "extraFee").as[Long] shouldBe CommonValidation.ScriptExtraFee
    }
  }

  routePath("/script/estimate") in {
    val base64 = ExprScript(script).explicitGet().bytes().base64

    Post(routePath("/script/estimate"), base64) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "script").as[String] shouldBe base64
      (json \ "scriptText").as[String] shouldBe "FUNCTION_CALL(Native(0),List(1, 2))" // [WAIT] s"(1 == 2)"
      (json \ "complexity").as[Long] shouldBe 3
      (json \ "extraFee").as[Long] shouldBe CommonValidation.ScriptExtraFee
    }
  }

  routePath("/seed") in {
    Get(routePath("/seed")) ~> route ~> check {
      val seed = Base58.tryDecodeWithLimit((responseAs[JsValue] \ "seed").as[String])
      seed shouldBe 'success
      seed.get.length shouldEqual UtilsApiRoute.DefaultSeedSize
    }
  }

  routePath("/seed/{length}") in forAll(Gen.posNum[Int]) { l =>
    if (l > UtilsApiRoute.MaxSeedSize) {
      Get(routePath(s"/seed/$l")) ~> route should produce(TooBigArrayAllocation)
    } else {
      Get(routePath(s"/seed/$l")) ~> route ~> check {
        val seed = Base58.tryDecodeWithLimit((responseAs[JsValue] \ "seed").as[String])
        seed shouldBe 'success
        seed.get.length shouldEqual l
      }
    }
  }

  for ((hash, f) <- Seq[(String, String => Array[Byte])](
         "secure" -> crypto.secureHash,
         "fast"   -> crypto.fastHash
       )) {
    val uri = routePath(s"/hash/$hash")
    uri in {
      forAll(Gen.alphaNumStr) { s =>
        Post(uri, s) ~> route ~> check {
          val r = responseAs[JsObject]
          (r \ "message").as[String] shouldEqual s
          (r \ "hash").as[String] shouldEqual Base58.encode(f(s))
        }
      }
    }
  }
}
