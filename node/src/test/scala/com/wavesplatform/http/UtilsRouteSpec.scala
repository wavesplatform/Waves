package com.wavesplatform.http

import com.google.protobuf.ByteString
import com.wavesplatform.api.http.ApiError.TooBigArrayAllocation
import com.wavesplatform.api.http.{ScriptWithImportsRequest, UtilsApiRoute}
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction, VerifierAnnotation, VerifierFunction}
import com.wavesplatform.lang.directives.values.{V2, V3}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.utils.Time
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.{JsObject, JsValue}

class UtilsRouteSpec extends RouteSpec("/utils") with RestAPISettingsHelper with PropertyChecks {
  private val estimator = ScriptEstimatorV2
  private val route = UtilsApiRoute(
    new Time {
      def correctedTime(): Long = System.currentTimeMillis()
      def getTimestamp(): Long  = System.currentTimeMillis()
    },
    restAPISettings,
    estimator
  ).route

  val script = FUNCTION_CALL(
    function = PureContext.eq.header,
    args = List(CONST_LONG(1), CONST_LONG(2))
  )

  val dappVer = DApp(
    meta = DAppMeta(),
    decs = List.empty,
    callableFuncs = List.empty,
    verifierFuncOpt = Some(
      VerifierFunction(VerifierAnnotation("tx"), FUNC("verify", List(), TRUE))
    )
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

  routePath("/script/meta") in {
    //Expression
    val exprBase64 = ExprScript(script).explicitGet().bytes().base64
    Post(routePath("/script/meta"), exprBase64) ~> route ~> check {
      val json = responseAs[JsValue]
      json.toString shouldBe "{}"
    }

    //DApp
    val dApp = DApp(
      DAppMeta(
        version = 1,
        List(
          CallableFuncSignature(ByteString.copyFrom(Array[Byte](1, 2, 4, 8))),
          CallableFuncSignature(ByteString.copyFrom(Array[Byte](8, 4, 2, 1))),
          CallableFuncSignature(ByteString.EMPTY)
        )
      ),
      List(
        LET("letName", CONST_BOOLEAN(true)),
        FUNC("funcName", List("arg1", "arg2"), CONST_BOOLEAN(false))
      ),
      List(
        CallableFunction(
          CallableAnnotation("func1"),
          FUNC("anotherFunc", List("a", "b", "c", "d"), CONST_BOOLEAN(true))
        ),
        CallableFunction(
          CallableAnnotation("func2"),
          FUNC("default", List("x", "y", "z", "w"), CONST_BOOLEAN(false))
        ),
        CallableFunction(
          CallableAnnotation("func3"),
          FUNC("default", List(), CONST_BOOLEAN(false))
        )
      ),
      Some(
        VerifierFunction(
          VerifierAnnotation("hmmm"),
          FUNC("funcAgain", List("arg"), CONST_BOOLEAN(false))
        )
      )
    )
    val dappBase64 = ContractScript(V3, dApp).explicitGet().bytes().base64
    Post(routePath("/script/meta"), dappBase64) ~> route ~> check {
      val json = responseAs[JsValue]
      json.toString shouldBe
        """{"callableFuncTypes":[
          |{"a":"Int","b":"ByteVector","c":"Boolean","d":"String"},
          |{"x":"String","y":"Boolean","z":"ByteVector","w":"Int"},
          |{}
          |]}
          |""".stripMargin
          .replace("\n", "")
    }
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
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }
  }

  routePath("/script/compileWithImports") in {
    Post(routePath("/script/compileWithImports"), ScriptWithImportsRequest("(1 == 2)")) ~> route ~> check {
      val json           = responseAs[JsValue]
      val expectedScript = ExprScript(V2, script).explicitGet()

      Script.fromBase64String((json \ "script").as[String]) shouldBe Right(expectedScript)
      (json \ "complexity").as[Long] shouldBe 3
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }

    val request = ScriptWithImportsRequest(
      """
        | {-# SCRIPT_TYPE ACCOUNT #-}
        | {-# IMPORT dir/my/lib.ride #-}
        | let a = 5
        | inc(a) == a + 1
      """.stripMargin,
      Map(
        "dir/my/lib.ride" ->
          """
            | {-# CONTENT_TYPE LIBRARY #-}
            | func inc(a: Int) = a + 1
          """.stripMargin
      )
    )
    Post(routePath("/script/compileWithImports"), request) ~> route ~> check {
      val expectedScript =
        """
          | {-# SCRIPT_TYPE ACCOUNT #-}
          | func inc(a: Int) = a + 1
          | let a = 5
          | inc(a) == a + 1
        """.stripMargin
      val compiled = ScriptCompiler.compile(expectedScript, ScriptEstimatorV2)

      val json         = responseAs[JsValue]
      val base64Result = Script.fromBase64String((json \ "script").as[String])
      base64Result shouldBe compiled.map(_._1)
      (json \ "complexity").as[Long] shouldBe compiled.map(_._2).explicitGet()
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
    }
  }

  routePath("/script/estimate") in {
    val base64 = ExprScript(script).explicitGet().bytes().base64

    Post(routePath("/script/estimate"), base64) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "script").as[String] shouldBe base64
      (json \ "scriptText").as[String] shouldBe "FUNCTION_CALL(Native(0),List(1, 2))" // [WAIT] s"(1 == 2)"
      (json \ "complexity").as[Long] shouldBe 3
      (json \ "extraFee").as[Long] shouldBe FeeValidation.ScriptExtraFee
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
