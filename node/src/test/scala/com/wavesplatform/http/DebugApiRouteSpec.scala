package com.wavesplatform.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import com.wavesplatform.api.http.ApiError.ApiKeyNotValid
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.network.PeerDatabase
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{Blockchain, LeaseBalance, NG}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.{NTPTime, TestValues, TestWallet}
import monix.eval.Task
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.Json

//noinspection ScalaStyle
class DebugApiRouteSpec extends RouteSpec("/debug") with RestAPISettingsHelper with TestWallet with NTPTime with PathMockFactory {

  private val sampleConfig  = com.typesafe.config.ConfigFactory.load()
  private val wavesSettings = WavesSettings.fromRootConfig(sampleConfig)
  private val configObject  = sampleConfig.root()
  private val debugApiRoute: DebugApiRoute = DebugApiRoute(
    wavesSettings,
    ntpTime,
    null,
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
  )
  private val route = debugApiRoute.route

  routePath("/configInfo") - {
    "requires api-key header" in {
      Get(routePath("/configInfo?full=true")) ~> route should produce(ApiKeyNotValid)
      Get(routePath("/configInfo?full=false")) ~> route should produce(ApiKeyNotValid)
    }
  }

  routePath("/validate") - {
    def createBlockchainStub(): Blockchain with NG = {
      trait Blockchain1 extends Blockchain with NG
      val blockchain = stub[Blockchain1]
      (blockchain.settings _).when().returns(WavesSettings.default().blockchainSettings)
      (blockchain.activatedFeatures _).when().returns(Map.empty)
      (blockchain.accountScript _).when(*).returns(None)
      (blockchain.leaseBalance _).when(*).returns(LeaseBalance.empty)
      (blockchain.height _).when().returns(1)
      (blockchain.blockHeader _).when(*).returns {
        val block = TestBlock.create(Nil)
        Some(SignedBlockHeader(block.header, block.signature))
      }
      blockchain
    }

    "valid tx" in {
      val blockchain = createBlockchainStub()
      (blockchain.balance _).when(TxHelpers.defaultSigner.publicKey.toAddress, *).returns(Long.MaxValue)

      val route = debugApiRoute.copy(blockchain = blockchain).route

      val tx = TxHelpers.transfer(TxHelpers.defaultSigner, TestValues.address, TestValues.OneWaves)
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

      val tx = TxHelpers.transfer(TxHelpers.defaultSigner, TestValues.address, TestValues.OneWaves)
      Post(routePath("/validate"), HttpEntity(ContentTypes.`application/json`, tx.json().toString())) ~> route ~> check {
        val json = Json.parse(responseAs[String])
        println(json)
        (json \ "valid").as[Boolean] shouldBe false
        (json \ "validationTime").as[Int] shouldBe 1000 +- 1000
        (json \ "error").as[String] should include("Attempt to transfer unavailable funds")
      }
    }
  }
}
