package com.wavesplatform.http

import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.http.BlocksApiRoute
import com.wavesplatform.block.Block
import com.wavesplatform.block.serialization.BlockHeaderSerializer
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.{NoShrink, TestWallet}
import monix.reactive.Observable
import org.scalamock.scalatest.PathMockFactory
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json._

class BlocksRouteSpec extends RouteSpec("/blocks") with PathMockFactory with PropertyChecks with RestAPISettingsHelper with TestWallet with NoShrink {
  private val blocksApi = mock[CommonBlocksApi]
  private val route     = BlocksApiRoute(restAPISettings, blocksApi).route

  private val testBlock1 = TestBlock.create(Nil)
  private val testBlock2 = TestBlock.create(Nil, Block.ProtoBlockVersion)

  private val testBlock1Json = testBlock1.json() ++ Json.obj("height" -> 1, "totalFee" -> 0L)
  private val testBlock2Json = testBlock2.json() ++ Json.obj("height" -> 2, "totalFee" -> 0L, "reward" -> 5, "VRF" -> testBlock2.id().toString)

  private val testBlock1HeaderJson = BlockHeaderSerializer.toJson(testBlock1.header, testBlock1.bytes().length, 0, testBlock1.signature) ++ Json.obj(
    "height"   -> 1,
    "totalFee" -> 0L
  )

  private val testBlock2HeaderJson = BlockHeaderSerializer.toJson(testBlock2.header, testBlock2.bytes().length, 0, testBlock2.signature) ++ Json.obj(
    "height"   -> 2,
    "totalFee" -> 0L,
    "reward"   -> 5,
    "VRF"      -> testBlock2.id().toString
  )

  private val testBlock1Meta = BlockMeta.fromBlock(testBlock1, 1, 0L, None, None)
  private val testBlock2Meta = BlockMeta.fromBlock(testBlock2, 2, 0L, Some(5), Some(testBlock2.id()))

  routePath("/first") in {
    (blocksApi.blockAtHeight _).expects(1).returning(Some(testBlock1Meta -> Seq.empty)).once()
    Get(routePath("/first")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1Json
      response
    }
  }

  routePath("/last") in {
    (() => blocksApi.currentHeight).expects().returning(2).once()
    (blocksApi.blockAtHeight _).expects(2).returning(Some(testBlock2Meta -> Seq.empty)).once()
    Get(routePath("/last")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2Json
      response
    }
  }

  routePath("/at/{height}") in {
    (blocksApi.blockAtHeight _).expects(1).returning(Some(testBlock1Meta -> Seq.empty)).once()
    Get(routePath("/at/1")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1Json
      response
    }

    (blocksApi.blockAtHeight _).expects(2).returning(Some(testBlock2Meta -> Seq.empty)).once()
    Get(routePath("/at/2")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2Json
      response
    }
  }

  routePath("/signature/{signature}") in {
    (blocksApi.block _).expects(testBlock1.id()).returning(Some(testBlock1Meta -> Seq.empty)).once()
    (blocksApi.block _).expects(testBlock2.id()).returning(Some(testBlock2Meta -> Seq.empty)).once()
    Get(routePath(s"/signature/${testBlock1.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1Json
      response
    }

    Get(routePath(s"/signature/${testBlock2.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2Json
      response
    }
  }

  routePath("/{id}") in {
    (blocksApi.block _).expects(testBlock1.id()).returning(Some(testBlock1Meta -> Seq.empty)).once()
    (blocksApi.block _).expects(testBlock2.id()).returning(Some(testBlock2Meta -> Seq.empty)).once()
    Get(routePath(s"/${testBlock1.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1Json
      response
    }

    Get(routePath(s"/${testBlock2.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2Json
      response
    }
  }

  routePath("/seq/{from}/{to}") in {
    (blocksApi
      .blocksRange(_: Int, _: Int))
      .expects(1, 2)
      .returning(
        Observable.fromIterable(
          Seq(
            testBlock1Meta -> Seq.empty,
            testBlock2Meta -> Seq.empty
          )
        )
      )
    Get(routePath("/seq/1/2")) ~> route ~> check {
      val response = responseAs[Seq[JsObject]]
      response shouldBe Seq(testBlock1Json, testBlock2Json)
      response
    }
  }

  routePath("/headers/last") in {
    (() => blocksApi.currentHeight).expects().returning(2).once()
    (blocksApi.metaAtHeight _).expects(2).returning(Some(testBlock2Meta)).once()
    Get(routePath("/headers/last")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2HeaderJson
      response
    }
  }

  routePath("/headers/{id}") in {
    (blocksApi.meta _).expects(testBlock1.id()).returning(Some(testBlock1Meta)).once()
    (blocksApi.meta _).expects(testBlock2.id()).returning(Some(testBlock2Meta)).once()

    Get(routePath(s"/headers/${testBlock1.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1HeaderJson
      response
    }

    Get(routePath(s"/headers/${testBlock2.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2HeaderJson
      response
    }
  }

  routePath("/headers/at/{height}") in {
    (blocksApi.metaAtHeight _).expects(1).returning(Some(testBlock1Meta)).once()
    (blocksApi.metaAtHeight _).expects(2).returning(Some(testBlock2Meta)).once()

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

  routePath("/headers/seq/{from}/{to}") in {
    (blocksApi.metaRange _)
      .expects(1, 2)
      .returning(
        Observable.fromIterable(
          Seq(
            testBlock1Meta,
            testBlock2Meta
          )
        )
      )
    Get(routePath("/headers/seq/1/2")) ~> route ~> check {
      val response = responseAs[Seq[JsObject]]
      response shouldBe Seq(testBlock1HeaderJson, testBlock2HeaderJson)
      response
    }
  }
}
