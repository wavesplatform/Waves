package com.wavesplatform.ride.app

import com.typesafe.config.{Config, ConfigRenderOptions}
import com.wavesplatform.account.Address
import com.wavesplatform.api.{DefaultBlockchainApi, GrpcChannelSettings, GrpcConnector, RideApi}
import com.wavesplatform.blockchain.BlockchainProcessor
import com.wavesplatform.ride.app.RideRunnerSettings.DbSettings
import com.wavesplatform.settings.*
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*
import net.ceedubs.ficus.readers.{CollectionReaders, ValueReader}
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.duration.FiniteDuration

case class RideRunnerGlobalSettings(
    rideRunner: RideRunnerSettings,
    blockchain: BlockchainSettings,
    restApi: RestAPISettings,
    compare: CompareSettings
)

object RideRunnerGlobalSettings {
  def fromRootConfig(config: Config): RideRunnerGlobalSettings = config.getConfig("waves").as[RideRunnerGlobalSettings]

  implicit val testRequestsValueReader: ValueReader[Map[Address, JsObject]] = CollectionReaders.mapValueReader[Config].map { xs =>
    xs.map { case (k, v) =>
      val key = Address.fromString(k) match {
        case Right(x) => x
        case Left(e)  => throw new RuntimeException(s"Can't parse key '$k' as Address: ${e.toString}")
      }

      val strV = v.root().render(ConfigRenderOptions.concise())
      val value = Json.parse(strV) match {
        case x: JsObject => x
        case x           => throw new RuntimeException(s"Can't parse value as JsObject: $x")
      }

      key -> value
    }
  }
}

case class RideRunnerSettings(
    db: DbSettings,
    unhealthyIdleTimeout: FiniteDuration,
    processor: BlockchainProcessor.Settings,
    blockchainApi: DefaultBlockchainApi.Settings,
    grpcConnector: GrpcConnector.Settings,
    grpcApiChannel: GrpcChannelSettings,
    blockchainUpdatesApiChannel: GrpcChannelSettings
) {
  val unhealthyIdleTimeoutMs = unhealthyIdleTimeout.toMillis
}

object RideRunnerSettings {
  case class DbSettings(directory: String)
}

case class CompareSettings(
    requestsDelay: FiniteDuration,
    failedChecksToleranceTimer: FiniteDuration,
    maxChecks: Option[Long],
    rideApi: RideApi.Settings,
    testRequests: Map[Address, JsObject]
)
