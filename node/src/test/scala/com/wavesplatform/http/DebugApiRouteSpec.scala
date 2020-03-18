package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.api.http.ApiError.ApiKeyNotValid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.StateHash.SectionId
import com.wavesplatform.state.{Blockchain, StateHash}
import com.wavesplatform.{NTPTime, TestWallet}
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.{JsObject, Json}

import scala.util.Random

//noinspection ScalaStyle
class DebugApiRouteSpec extends RouteSpec("/debug") with RestAPISettingsHelper with TestWallet with NTPTime with PathMockFactory {

  val wavesSettings = WavesSettings.default()
  val configObject  = wavesSettings.config.root()
  val blockchain    = stub[Blockchain]
  val block         = TestBlock.create(Nil)
  val testStateHash = {
    def randomHash: ByteStr = Array.fill(32)(Random.nextInt(256).toByte)
    val hashes              = SectionId.values.map((_, randomHash)).toMap
    StateHash(randomHash, hashes)
  }

  val debugApiRoute =
    DebugApiRoute(wavesSettings, ntpTime, blockchain, null, null, null, null, null, null, null, null, null, null, null, configObject, _ => Seq.empty, {
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
      (blockchain.blockBytes(_: Int)).when(*).returning(Some(block.bytes()))
      Get(routePath("/stateHash/2")) ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[JsObject] shouldBe (Json.toJson(testStateHash).as[JsObject] ++ Json.obj("blockId" -> block.uniqueId.toString))
      }

      Get(routePath("/stateHash/3")) ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }
  }
}
