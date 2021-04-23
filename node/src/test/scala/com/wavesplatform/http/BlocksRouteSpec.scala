package com.wavesplatform.http

import scala.util.Random

import com.wavesplatform.{NoShrink, TestWallet}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.http.ApiMarshallers._
import com.wavesplatform.api.http.BlocksApiRoute
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.block.serialization.BlockHeaderSerializer
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.TxHelpers
import monix.reactive.Observable
import org.scalamock.scalatest.PathMockFactory
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json._

class BlocksRouteSpec extends RouteSpec("/blocks") with PathMockFactory with PropertyChecks with RestAPISettingsHelper with TestWallet with NoShrink {
  private val blocksApi                      = mock[CommonBlocksApi]
  private val blocksApiRoute: BlocksApiRoute = BlocksApiRoute(restAPISettings, blocksApi)
  private val route                          = blocksApiRoute.route

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

  private val invalidBlockId = ByteStr(new Array[Byte](32))
  (blocksApi.block _).expects(invalidBlockId).returning(None).anyNumberOfTimes()
  (blocksApi.meta _).expects(invalidBlockId).returning(None).anyNumberOfTimes()

  routePath("/first") in {
    (blocksApi.blockAtHeight _).expects(1).returning(Some(testBlock1Meta -> Seq.empty)).once()
    Get(routePath("/first")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1Json
    }
  }

  routePath("/last") in {
    (() => blocksApi.currentHeight).expects().returning(2).once()
    (blocksApi.blockAtHeight _).expects(2).returning(Some(testBlock2Meta -> Seq.empty)).once()
    Get(routePath("/last")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2Json
    }
  }

  routePath("/at/{height}") in {
    (blocksApi.blockAtHeight _).expects(1).returning(Some(testBlock1Meta -> Seq.empty)).once()
    Get(routePath("/at/1")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1Json
    }

    (blocksApi.blockAtHeight _).expects(2).returning(Some(testBlock2Meta -> Seq.empty)).once()
    Get(routePath("/at/2")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2Json
    }
  }

  routePath("/signature/{signature}") in {
    (blocksApi.block _).expects(testBlock1.id()).returning(Some(testBlock1Meta -> Seq.empty)).once()
    (blocksApi.block _).expects(testBlock2.id()).returning(Some(testBlock2Meta -> Seq.empty)).once()
    Get(routePath(s"/signature/${testBlock1.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1Json
    }

    Get(routePath(s"/signature/${testBlock2.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2Json
    }

    Get(routePath(s"/signature/$invalidBlockId")) ~> route ~> check {
      response.status.isFailure() shouldBe true
      responseAs[String] should include("block does not exist")
    }
  }

  routePath("/{id}") in {
    (blocksApi.block _).expects(testBlock1.id()).returning(Some(testBlock1Meta -> Seq.empty)).once()
    (blocksApi.block _).expects(testBlock2.id()).returning(Some(testBlock2Meta -> Seq.empty)).once()

    Get(routePath(s"/${testBlock1.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1Json
    }

    Get(routePath(s"/${testBlock2.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2Json
    }

    Get(routePath(s"/$invalidBlockId")) ~> route ~> check {
      response.status.isFailure() shouldBe true
      responseAs[String] should include("block does not exist")
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
    }
  }

  routePath("/headers/last") in {
    (() => blocksApi.currentHeight).expects().returning(2).once()
    (blocksApi.metaAtHeight _).expects(2).returning(Some(testBlock2Meta)).once()
    Get(routePath("/headers/last")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2HeaderJson
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
    }

    Get(routePath(s"/headers/$invalidBlockId")) ~> route ~> check {
      response.status.isFailure() shouldBe true
      responseAs[String] should include("block does not exist")
    }
  }

  routePath("/headers/at/{height}") in {
    (blocksApi.metaAtHeight _).expects(1).returning(Some(testBlock1Meta)).once()
    (blocksApi.metaAtHeight _).expects(2).returning(Some(testBlock2Meta)).once()

    Get(routePath("/headers/at/1")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1HeaderJson
    }

    Get(routePath("/headers/at/2")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2HeaderJson
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
    }
  }

  routePath("/delay/{blockId}/{number}") in {
    val blocks = Vector(
      Block(BlockHeader(1, 0, ByteStr.empty, 0, ByteStr.empty, TxHelpers.defaultSigner.publicKey, Nil, 0, ByteStr.empty), ByteStr(Random.nextBytes(64)), Nil),
      Block(BlockHeader(1, 1000, ByteStr.empty, 0, ByteStr.empty, TxHelpers.defaultSigner.publicKey, Nil, 0, ByteStr.empty), ByteStr(Random.nextBytes(64)), Nil),
      Block(BlockHeader(1, 2000, ByteStr.empty, 0, ByteStr.empty, TxHelpers.defaultSigner.publicKey, Nil, 0, ByteStr.empty), ByteStr(Random.nextBytes(64)), Nil)
    )

    val blockchain = stub[Blockchain]
    (blockchain.heightOf _).when(blocks.last.id()).returning(Some(3))

    def metaAt(height: Int): Option[BlockMeta] =
      if (height >= 1 && height <= 3)
        Some(BlockMeta(blocks(height - 1).header, ByteStr.empty, None, 1, 0, 0, 0, None, None))
      else None

    val blocksApi = CommonBlocksApi(blockchain, metaAt, _ => None)
    val route = blocksApiRoute.copy(commonApi = blocksApi).route
    Get(routePath(s"/delay/${blocks.last.id()}/3")) ~> route ~> check {
      val delay = (responseAs[JsObject] \ "delay").as[Int]
      delay shouldBe 1000
    }

    Get(routePath(s"/delay/${blocks.last.id()}/1")) ~> route ~> check {
      val delay = (responseAs[JsObject] \ "delay").as[Int]
      delay shouldBe 1000
    }
  }
}
