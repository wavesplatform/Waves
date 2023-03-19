package com.wavesplatform.ride.runner.entrypoints

import com.typesafe.config.{Config, ConfigList, ConfigRenderOptions, ConfigValue}
import com.wavesplatform.account.Address
import com.wavesplatform.api.{DefaultBlockchainApi, GrpcChannelSettings, GrpcConnector}
import com.wavesplatform.ride.runner.db.RideDb
import com.wavesplatform.ride.runner.storage.SharedBlockchainStorage
import com.wavesplatform.ride.runner.{BlockchainState, DefaultRequestService}
import com.wavesplatform.settings.*
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*
import net.ceedubs.ficus.readers.{CollectionReaders, ValueReader}
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.duration.FiniteDuration

case class RideRunnerGlobalSettings(
    restApi: RestAPISettings,
    rideRunner: RideRunnerCommonSettings,
    rideCompareService: RideCompareService.Settings
)

object RideRunnerGlobalSettings {
  def fromRootConfig(config: Config): RideRunnerGlobalSettings = config.getConfig("waves").as[RideRunnerGlobalSettings]

  implicit val testRequestsValueReader: ValueReader[List[(Address, JsObject)]] = CollectionReaders
    .traversableReader[List, ConfigValue]
    .map { xs =>
      xs.map {
        case xs: ConfigList if xs.unwrapped().size() == 2 =>
          val key = xs.get(0).unwrapped() match {
            case k: String =>
              Address.fromString(k) match {
                case Right(x) => x
                case Left(e)  => throw new RuntimeException(s"Can't parse '$k' as Address: ${e.toString}")
              }
            case k => throw new RuntimeException(s"Can't parse as Address: $k")
          }

          val strV = xs.get(1).render(ConfigRenderOptions.concise())
          val value = Json.parse(strV) match {
            case x: JsObject => x
            case x           => throw new RuntimeException(s"Can't parse value as JsObject: $x")
          }

          key -> value

        case xs => throw new RuntimeException(s"Expected two elements, got: $xs")
      }
    }
}

case class RideRunnerCommonSettings(
    db: RideDb.Settings,
    unhealthyIdleTimeout: FiniteDuration,
    rideSchedulerThreads: Option[Int],
    immutableBlockchain: BlockchainSettings,
    sharedBlockchain: SharedBlockchainStorage.Settings,
    blockchainState: BlockchainState.Settings,
    requestsService: DefaultRequestService.Settings,
    blockchainApi: DefaultBlockchainApi.Settings,
    grpcConnector: GrpcConnector.Settings,
    grpcApiChannel: GrpcChannelSettings,
    blockchainUpdatesApiChannel: GrpcChannelSettings
) {
  val unhealthyIdleTimeoutMs    = unhealthyIdleTimeout.toMillis
  val exactRideSchedulerThreads = rideSchedulerThreads.getOrElse(Runtime.getRuntime.availableProcessors() * 2).min(4)
}
