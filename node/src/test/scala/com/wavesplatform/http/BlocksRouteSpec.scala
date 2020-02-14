package com.wavesplatform.http

import com.wavesplatform.api.http.BlocksApiRoute
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockInfo
import com.wavesplatform.block.serialization.BlockHeaderSerializer
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.state.Blockchain
import com.wavesplatform.{NoShrink, TestWallet}
import org.scalamock.scalatest.PathMockFactory
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json._

class BlocksRouteSpec extends RouteSpec("/blocks") with PathMockFactory with PropertyChecks with RestAPISettingsHelper with TestWallet with NoShrink {
  private val blockchain = stub[Blockchain]

  private val route =
    BlocksApiRoute(restAPISettings, blockchain).route

  val testBlock1 = TestBlock.create(Nil)
  val testBlock2 = TestBlock.create(Nil, Block.ProtoBlockVersion)

  val testBlock1Json = testBlock1.json() ++ Json.obj("height" -> 1, "totalFee" -> 10L)
  val testBlock2Json = testBlock2.json() ++ Json.obj("height" -> 2, "totalFee" -> 10L, "reward" -> 5, "VRF" -> testBlock2.uniqueId.toString)

  val testBlock1HeaderJson = BlockHeaderSerializer.toJson(testBlock1.header, testBlock1.bytes().size, 0, testBlock1.signature) ++ Json.obj(
    "height"   -> 1,
    "totalFee" -> 10L
  )
  val testBlock2HeaderJson = BlockHeaderSerializer.toJson(testBlock2.header, testBlock2.bytes().size, 0, testBlock2.signature) ++ Json.obj(
    "height"   -> 2,
    "totalFee" -> 10L,
    "reward"   -> 5,
    "VRF"      -> testBlock2.uniqueId.toString
  )

  (blockchain.blockBytes(_: Int)).when(1).returning(Some(testBlock1.bytes()))
  (blockchain.blockBytes(_: Int)).when(2).returning(Some(testBlock2.bytes()))

  (blockchain.blockInfo(_: Int)).when(1).returning(Some(BlockInfo(testBlock1.header, testBlock1.bytes().size, 0, testBlock1.signature)))
  (blockchain.blockInfo(_: Int)).when(2).returning(Some(BlockInfo(testBlock2.header, testBlock2.bytes().size, 0, testBlock2.signature)))

  (blockchain.blockBytes(_: ByteStr)).when(testBlock1.uniqueId).returning(Some(testBlock1.bytes()))
  (blockchain.blockBytes(_: ByteStr)).when(testBlock2.uniqueId).returning(Some(testBlock2.bytes()))

  (blockchain.heightOf(_: ByteStr)).when(testBlock1.uniqueId).returning(Some(1))
  (blockchain.heightOf(_: ByteStr)).when(testBlock2.uniqueId).returning(Some(2))

  (blockchain.lastBlock _).when().returning(Some(testBlock2))

  (blockchain.blockReward _).when(*).returning(Some(5L))
  (blockchain.totalFee _).when(*).returning(Some(10L))
  (blockchain.hitSourceAtHeight _).when(2).returning(Some(testBlock2.uniqueId))

  (blockchain.activatedFeatures _)
    .when()
    .returning(Map(BlockchainFeatures.BlockV5.id -> 2, BlockchainFeatures.BlockReward.id -> 2))
  (blockchain.height _).when().returning(2).anyNumberOfTimes()

  routePath("/first") in {
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
