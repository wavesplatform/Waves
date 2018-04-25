package com.wavesplatform.http

import com.wavesplatform.crypto
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.Terms.{BOOLEAN, Typed}
import com.wavesplatform.settings.{FeesSettings, SmartAccountSettings}
import com.wavesplatform.state.EitherExt2
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, JsValue}
import scorex.api.http.{TooBigArrayAllocation, UtilsApiRoute}
import scorex.crypto.encode.Base58
import scorex.transaction.smart.script.Script
import scorex.transaction.smart.script.v1.ScriptV1
import scorex.utils.Time

class UtilsRouteSpec extends RouteSpec("/utils") with RestAPISettingsHelper with PropertyChecks {
  private val route = UtilsApiRoute(
    new Time {
      def correctedTime(): Long = System.currentTimeMillis()
      def getTimestamp(): Long  = System.currentTimeMillis()
    },
    restAPISettings,
    FeesSettings(SmartAccountSettings(1000, 2), Map.empty)
  ).route

  routePath("/script/compile") in {
    Post(routePath("/script/compile"), "1 == 2") ~> route ~> check {
      val json = responseAs[JsValue]
      val expectedScript = ScriptV1(
        Typed.FUNCTION_CALL(
          function = FunctionHeader("==", List(FunctionHeader.FunctionHeaderType.LONG, FunctionHeader.FunctionHeaderType.LONG)),
          args = List(Typed.CONST_LONG(1), Typed.CONST_LONG(2)),
          tpe = BOOLEAN
        )).explicitGet()

      Script.fromBase58String((json \ "script").as[String]) shouldBe Right(expectedScript)
      (json \ "complexity").as[Long] shouldBe 3
      (json \ "extraFee").as[Long] shouldBe 1006
    }
  }

  routePath("/script/estimate") in {
    Post(routePath("/script/estimate"), "ENDztao2K4J3jX4YPmCkBynkAeuK7ZXgDLF22c9FPhC74jDE3DQKMfT") ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "script").as[String] shouldBe "ENDztao2K4J3jX4YPmCkBynkAeuK7ZXgDLF22c9FPhC74jDE3DQKMfT"
      (json \ "scriptText").as[String] shouldBe "FUNCTION_CALL(FunctionHeader(==,List(LONG, LONG)),List(CONST_LONG(1), CONST_LONG(2)),BOOLEAN)"
      (json \ "complexity").as[Long] shouldBe 3
      (json \ "extraFee").as[Long] shouldBe 1006
    }
  }

  routePath("/seed") in {
    Get(routePath("/seed")) ~> route ~> check {
      val seed = Base58.decode((responseAs[JsValue] \ "seed").as[String])
      seed shouldBe 'success
      seed.get.length shouldEqual UtilsApiRoute.DefaultSeedSize
    }
  }

  routePath("/seed/{length}") in forAll(Gen.posNum[Int]) { l =>
    if (l > UtilsApiRoute.MaxSeedSize) {
      Get(routePath(s"/seed/$l")) ~> route should produce(TooBigArrayAllocation)
    } else {
      Get(routePath(s"/seed/$l")) ~> route ~> check {
        val seed = Base58.decode((responseAs[JsValue] \ "seed").as[String])
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
