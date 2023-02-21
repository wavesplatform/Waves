package com.wavesplatform.riderunner.app

import com.typesafe.config.{Config, ConfigList, ConfigRenderOptions, ConfigValue}
import com.wavesplatform.account.Address
import com.wavesplatform.api.{DefaultBlockchainApi, GrpcChannelSettings, GrpcConnector, RideApi}
import com.wavesplatform.blockchain.SharedBlockchainData
import com.wavesplatform.http.ServiceApiRoute
import com.wavesplatform.riderunner.DefaultRequestsService
import com.wavesplatform.riderunner.app.RideRunnerSettings.DbSettings
import com.wavesplatform.settings.*
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*
import net.ceedubs.ficus.readers.{CollectionReaders, ValueReader}
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

case class RideRunnerGlobalSettings(
    rideRunner: RideRunnerSettings,
    restApi: RestAPISettings,
    compare: CompareSettings
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

case class RideRunnerSettings(
    db: DbSettings,
    unhealthyIdleTimeout: FiniteDuration,
    rideSchedulerThreads: Option[Int],
    serviceApiRoute: ServiceApiRoute.Settings,
    immutableBlockchain: BlockchainSettings,
    sharedBlockchain: SharedBlockchainData.Settings,
    requestsService: DefaultRequestsService.Settings,
    blockchainApi: DefaultBlockchainApi.Settings,
    grpcConnector: GrpcConnector.Settings,
    grpcApiChannel: GrpcChannelSettings,
    blockchainUpdatesApiChannel: GrpcChannelSettings
) {
  val unhealthyIdleTimeoutMs = unhealthyIdleTimeout.toMillis
  val exactRideSchedulerThreads = rideSchedulerThreads.getOrElse(Runtime.getRuntime.availableProcessors() * 2).min(4)
}

object RideRunnerSettings {
  case class DbSettings(directory: String) {
    def toNode: DBSettings = DBSettings(
      directory = directory,
      storeTransactionsByAddress = false,
      storeInvokeScriptResults = false,
      storeStateHashes = false,
      maxCacheSize = 0,
      maxRollbackDepth = 0,
      rememberBlocks = 0.seconds,
      useBloomFilter = false,
      inMemory = InMemorySettings(SizeInBytes(0L), SizeInBytes(0L))
    )
  }
}

case class CompareSettings(
    requestsDelay: FiniteDuration,
    failedChecksToleranceTimer: FiniteDuration,
    maxChecks: Option[Long],
    rideApi: RideApi.Settings,
    testRequests: List[(Address, JsObject)]
)
