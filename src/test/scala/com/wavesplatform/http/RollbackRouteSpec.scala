package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.api.http.CustomValidationError
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{Blockchain, ByteStr, NG}
import com.wavesplatform.utils.Base58
import org.scalamock.scalatest.MockFactory
import play.api.libs.json.Json

import scala.util.Random

class RollbackRouteSpec extends RouteSpec("/debug") with MockFactory with RestAPISettingsHelper {
  private val sampleConfig  = com.typesafe.config.ConfigFactory.load()
  private val wavesSettings = WavesSettings.fromConfig(sampleConfig).copy(restAPISettings = restAPISettings)
  private val configObject  = sampleConfig.root()

  val signatureHeight = 10
  val (signature, signatureString): (ByteStr, String) = {
    val bytes = Array.fill(64)(0: Byte)
    Random.nextBytes(bytes)
    (ByteStr(bytes), Base58.encode(bytes))
  }

  "DebugApiRoute" - {
    s"can't rollback to more than ${LevelDBWriter.MAX_DEPTH}" in {
      val blockchain = stub[Blockchain]
      val ng         = stub[NG]

      val route =
        DebugApiRoute(wavesSettings, blockchain, null, ng, null, null, null, null, null, null, null, null, null, null, configObject).route

      val params = Json.toJson(RollbackParams(signatureHeight, false))

      ng.height _ when () returns 2001
      blockchain.height _ when () returns 2001
      blockchain.heightOf _ when signature returns Some(signatureHeight)

      Delete(routePath(s"/rollback-to/$signatureString")) ~>
        api_key(apiKey) ~>
        route should produce(CustomValidationError("Rollback of more than 1990 blocks is forbidden"))

      Post(routePath("/rollback"), params) ~>
        api_key(apiKey) ~>
        route ~>
        check {
          status shouldBe StatusCodes.BadRequest
          responseAs[String] shouldBe "Rollback of more than 1990 blocks is forbidden"
        }
    }
  }

}
