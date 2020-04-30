package com.wavesplatform.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import com.wavesplatform.api.http.ApiError.ApiKeyNotValid
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.util._
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.network.PeerDatabase
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.StateHash.SectionId
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, Blockchain, Height, LeaseBalance, NG, StateHash, VolumeAndFee}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.assets.exchange.OrderType
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.{NTPTime, TestValues, TestWallet}
import monix.eval.Task
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.{JsObject, Json}

import scala.util.Random

//noinspection ScalaStyle
class DebugApiRouteSpec extends RouteSpec("/debug") with RestAPISettingsHelper with TestWallet with NTPTime with PathMockFactory {

  val wavesSettings = WavesSettings.default()
  val configObject  = wavesSettings.config.root()
  trait Blockchain1 extends Blockchain with NG
  val blockchain    = stub[Blockchain1]
  val block         = TestBlock.create(Nil)
  val testStateHash = {
    def randomHash: ByteStr = ByteStr(Array.fill(32)(Random.nextInt(256).toByte))
    val hashes              = SectionId.values.map((_, randomHash)).toMap
    StateHash(randomHash, hashes)
  }

  val debugApiRoute =
    DebugApiRoute(
      wavesSettings,
      ntpTime,
      blockchain,
      null,
      null,
      null,
      null,
      PeerDatabase.NoOp,
      null,
      (_, _) => Task.raiseError(new NotImplementedError("")),
      null,
      null,
      null,
      null,
      null,
      null,
      configObject,
      _ => Seq.empty
    , {
      case 2 => Some(testStateHash)
      case _ => None
    })
  import debugApiRoute._

  routePath("/configInfo") - {
    "requires api-key header" in {
      Get(routePath("/configInfo?full=true")) ~> route should produce(ApiKeyNotValid)
      Get(routePath("/configInfo?full=false")) ~> route should produce(ApiKeyNotValid)
    }
  }

  routePath("/stateHash") - {
    "works" in {
      (blockchain.blockHeader(_: Int)).when(*).returning(Some(SignedBlockHeader(block.header, block.signature)))
      Get(routePath("/stateHash/2")) ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[JsObject] shouldBe (Json.toJson(testStateHash).as[JsObject] ++ Json.obj("blockId" -> block.id().toString))
      }

      Get(routePath("/stateHash/3")) ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }
  }

  routePath("/validate") - {
    def createBlockchainStub(): Blockchain with NG = {
      trait Blockchain1 extends Blockchain with NG
      val blockchain = stub[Blockchain1]
      (() => blockchain.settings).when().returns(WavesSettings.default().blockchainSettings)
      (() => blockchain.activatedFeatures)
        .when()
        .returns(
          Map(
            BlockchainFeatures.BlockV5.id             -> 0,
            BlockchainFeatures.SmartAccounts.id       -> 0,
            BlockchainFeatures.SmartAssets.id         -> 0,
            BlockchainFeatures.SmartAccountTrading.id -> 0,
            BlockchainFeatures.OrderV3.id             -> 0
          )
        )
      (blockchain.accountScript _).when(*).returns(None)
      (blockchain.leaseBalance _).when(*).returns(LeaseBalance.empty)
      (() => blockchain.height).when().returns(1)
      (blockchain.blockHeader _).when(*).returns {
        val block = TestBlock.create(Nil)
        Some(SignedBlockHeader(block.header, block.signature))
      }
      (blockchain.filledVolumeAndFee _).when(*).returns(VolumeAndFee.empty)
      blockchain
    }

    "valid tx" in {
      val blockchain = createBlockchainStub()
      (blockchain.balance _).when(TxHelpers.defaultSigner.publicKey.toAddress, *).returns(Long.MaxValue)

      val route = debugApiRoute.copy(blockchain = blockchain).route

      val tx = TxHelpers.transfer(TxHelpers.defaultSigner, TestValues.address, 1.waves)
      Post(routePath("/validate"), HttpEntity(ContentTypes.`application/json`, tx.json().toString())) ~> route ~> check {
        val json = Json.parse(responseAs[String])
        (json \ "valid").as[Boolean] shouldBe true
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
      }
    }

    "invalid tx" in {
      val blockchain = createBlockchainStub()
      (blockchain.balance _).when(TxHelpers.defaultSigner.publicKey.toAddress, *).returns(0)

      val route = debugApiRoute.copy(blockchain = blockchain).route

      val tx = TxHelpers.transfer(TxHelpers.defaultSigner, TestValues.address, 1.waves)
      Post(routePath("/validate"), HttpEntity(ContentTypes.`application/json`, tx.json().toString())) ~> route ~> check {
        val json = Json.parse(responseAs[String])
        (json \ "valid").as[Boolean] shouldBe false
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
        (json \ "error").as[String] should include("Attempt to transfer unavailable funds")
      }
    }

    "exchange tx with fail script" in {
      val blockchain = createBlockchainStub()
      (blockchain.balance _).when(TxHelpers.defaultSigner.publicKey.toAddress, *).returns(Long.MaxValue)

      val (assetScript, comp) = ScriptCompiler.compile("if true then throw(\"error\") else false", ScriptEstimatorV3).explicitGet()
      (blockchain.assetScript _).when(TestValues.asset).returns(Some(AssetScriptInfo(assetScript, comp)))
      (blockchain.assetDescription _)
        .when(TestValues.asset)
        .returns(
          Some(
            AssetDescription(null, null, null, null, 0, reissuable = false, null, Height(1), Some(AssetScriptInfo(assetScript, comp)), 0, nft = false)
          )
        )

      val route = debugApiRoute.copy(blockchain = blockchain).route

      val tx = TxHelpers.exchange(TxHelpers.order(OrderType.BUY, TestValues.asset), TxHelpers.order(OrderType.SELL, TestValues.asset))
      Post(routePath("/validate"), HttpEntity(ContentTypes.`application/json`, tx.json().toString())) ~> ApiKeyHeader ~> route ~> check {
        val json = Json.parse(responseAs[String])
        println(json)
        (json \ "valid").as[Boolean] shouldBe false
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
        (json \ "error").as[String] should include("not allowed by script of the asset")
      }
    }
  }
}
