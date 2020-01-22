package com.wavesplatform.http

import com.wavesplatform.api.http.BlocksApiRoute
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
  (blockchain.activatedFeatures _).when().returning(Map())

  private val route =
    BlocksApiRoute(restAPISettings, blockchain).route

  val testBlock1 = TestBlock.create(Nil)
  val testBlock2 = TestBlock.create(Nil)

  val testBlock1Json = testBlock1.json() ++ Json.obj("height" -> 1, "totalFee" -> 10L)
  val testBlock2Json = testBlock2.json() ++ Json.obj("height" -> 2, "totalFee" -> 10L, "VRF" -> testBlock2.uniqueId.toString)

  (blockchain.blockBytes(_: Int)).when(1).returning(Some(testBlock1.bytes()))
  (blockchain.blockBytes(_: Int)).when(2).returning(Some(testBlock2.bytes()))

  (blockchain.blockBytes(_: ByteStr)).when(testBlock1.uniqueId).returning(Some(testBlock1.bytes()))
  (blockchain.blockBytes(_: ByteStr)).when(testBlock2.uniqueId).returning(Some(testBlock2.bytes()))
  (blockchain.lastBlock _).when().returning(Some(testBlock2))

  (blockchain.blockReward _).when(*).returning(Some(5L))
  (blockchain.totalFee _).when(*).returning(Some(10L))
  (blockchain.hitSourceAtHeight _).when(2).returning(Some(testBlock2.uniqueId))

  (blockchain.activatedFeatures _)
    .when()
    .returning(Map(BlockchainFeatures.BlockV5.id -> 2, BlockchainFeatures.BlockReward.id -> 2))
    .anyNumberOfTimes()
  (blockchain.height _).when().returning(2).anyNumberOfTimes()

  println(blockchain.activatedFeatures)

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
}
