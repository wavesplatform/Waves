package com.wavesplatform.http

import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.http.BlocksApiRoute
import com.wavesplatform.block.serialization.BlockHeaderSerializer
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.{NoShrink, TestWallet}
import org.scalamock.scalatest.PathMockFactory
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json._

class BlocksRouteSpec extends RouteSpec("/blocks") with PathMockFactory with PropertyChecks with RestAPISettingsHelper with TestWallet with NoShrink {
  private val blocksApi = mock[CommonBlocksApi]
  private val route = BlocksApiRoute(restAPISettings, blocksApi).route

  val testBlock1 = TestBlock.create(Nil)
  val testBlock2 = TestBlock.create(Nil)

  val testBlock1Json = testBlock1.json() ++ Json.obj("height" -> 1, "totalFee" -> 10L)
  val testBlock2Json = testBlock2.json() ++ Json.obj("height" -> 2, "totalFee" -> 10L, "reward" -> 5, "VRF" -> testBlock2.uniqueId.toString)

  val testBlock1HeaderJson = BlockHeaderSerializer.toJson(testBlock1.header, testBlock1.bytes().length, 0, testBlock1.signature) ++ Json.obj(
    "height"   -> 1,
    "totalFee" -> 10L
  )
  val testBlock2HeaderJson = BlockHeaderSerializer.toJson(testBlock2.header, testBlock2.bytes().length, 0, testBlock2.signature) ++ Json.obj(
    "height"   -> 2,
    "totalFee" -> 10L,
    "reward"   -> 5,
    "VRF"      -> testBlock2.uniqueId.toString
  )

  routePath("/first") in {
    (blocksApi.blockAtHeight _).when(1).returns(Some(BlockMeta))
    Get(routePath("/first")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1Json
      response
    }
  }

  routePath("/last") in {
    Get(routePath("/last")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2Json
      response
    }
  }

  routePath("/at") in {
    Get(routePath("/at/1")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1Json
      response
    }

    Get(routePath("/at/2")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2Json
      response
    }
  }

  routePath("/signature") in {
    Get(routePath(s"/signature/${testBlock1.uniqueId}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1Json
      response
    }

    Get(routePath(s"/signature/${testBlock2.uniqueId}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2Json
      response
    }
  }

  routePath("/seq/1/2") in {
    Get(routePath("/seq/1/2")) ~> route ~> check {
      val response = responseAs[Seq[JsObject]]
      response shouldBe Seq(testBlock1Json, testBlock2Json)
      response
    }
  }

  routePath("/headers/last") in {
    Get(routePath("/headers/last")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2HeaderJson
      response
    }
  }

  routePath("/headers/at") in {
    Get(routePath("/headers/at/1")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1HeaderJson
      response
    }

    Get(routePath("/headers/at/2")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2HeaderJson
      response
    }
  }

  routePath("/headers/seq/1/2") in {
    Get(routePath("/headers/seq/1/2")) ~> route ~> check {
      val response = responseAs[Seq[JsObject]]
      response shouldBe Seq(testBlock1HeaderJson, testBlock2HeaderJson)
      response
    }
  }
}
