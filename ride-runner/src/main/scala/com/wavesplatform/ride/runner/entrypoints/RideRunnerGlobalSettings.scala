package com.wavesplatform.ride.runner.entrypoints

import com.typesafe.config.*
import com.wavesplatform.account.Address
import com.wavesplatform.api.{DefaultBlockchainApi, GrpcChannelSettings}
import com.wavesplatform.ride.runner.BlockchainState
import com.wavesplatform.ride.runner.db.RideRocksDb
import com.wavesplatform.ride.runner.requests.DefaultRequestService
import com.wavesplatform.ride.runner.storage.{InMemBlockchainDataCache, SharedBlockchainStorage}
import com.wavesplatform.settings.*
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*
import net.ceedubs.ficus.readers.{CollectionReaders, ValueReader}
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

case class RideRunnerGlobalSettings(
    publicApi: WavesPublicApiSettings,
    blockchain: BlockchainSettings,
    restApi: RestAPISettings,
    rideRunner: RideRunnerCommonSettings,
    rideCompareService: WavesRideRunnerCompareService.Settings
) {
  // Consider the service as unhealthy if it don't update events in more than this duration.
  // Should be more than publicApi.noDataTimeout, because it could be fixed after a restart of the blockchain updates stream.
  val unhealthyIdleTimeoutMs: Long = (publicApi.noDataTimeout + 30.seconds).toMillis

  val heightsSettings = Heights.Settings(rideRunner.onEmptyStartFrom, blockchain.functionalitySettings)

  val sharedBlockchain = SharedBlockchainStorage.Settings(
    blockchain = blockchain,
    blockchainDataCache = rideRunner.blockchainDataCache
  )

  val blockchainApi = DefaultBlockchainApi.Settings(
    grpcApi = DefaultBlockchainApi.GrpcApiSettings(maxConcurrentRequests = rideRunner.grpcApiMaxConcurrentRequests),
    blockchainUpdatesApi = DefaultBlockchainApi.BlockchainUpdatesApiSettings(
      noDataTimeout = publicApi.noDataTimeout,
      bufferSize = rideRunner.blockchainBlocksBufferSize
    )
  )

  val blockchainState = BlockchainState.Settings(delayBeforeForceRestartBlockchainUpdates = rideRunner.delayBeforeForceRestartBlockchainUpdates)

  val requestsService = DefaultRequestService.Settings(
    enableTraces = rideRunner.enableTraces,
    enableStateChanges = rideRunner.enableStateChanges,
    evaluateScriptComplexityLimit = rideRunner.complexityLimit,
    maxTxErrorLogSize = rideRunner.maxTxErrorLogSize.toBytes.toInt,
    parallelRideRunThreads = rideRunner.parallelRideRunThreads.getOrElse(math.min(4, Runtime.getRuntime.availableProcessors() * 2)),
    cacheSize = rideRunner.responseCache.size,
    cacheTtl = rideRunner.responseCache.ttl,
    ignoredCleanupThreshold = rideRunner.responseCache.gcThreshold
  )
}

object RideRunnerGlobalSettings {
  def fromRootConfig(config: Config): RideRunnerGlobalSettings = config.getConfig("waves").as[RideRunnerGlobalSettings]

  implicit val configMemorySizeValueReader: ValueReader[ConfigMemorySize] = (config: Config, path: String) => config.getMemorySize(path)

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
    db: RideRocksDb.Settings,
    enableTraces: Boolean,
    enableStateChanges: Boolean,
    complexityLimit: Int,
    maxTxErrorLogSize: ConfigMemorySize,
    onEmptyStartFrom: Option[Int],
    responseCache: RideRunnerResponseCacheSettings,
    blockchainDataCache: InMemBlockchainDataCache.Settings,
    parallelRideRunThreads: Option[Int],
    rideSchedulerThreads: Option[Int],
    blockchainBlocksBufferSize: Int,
    grpcApiMaxConcurrentRequests: Option[Int],
    grpcApiChannel: GrpcChannelSettings,
    blockchainUpdatesApiChannel: GrpcChannelSettings,
    delayBeforeForceRestartBlockchainUpdates: FiniteDuration
) {
  val availableProcessors          = Runtime.getRuntime.availableProcessors()
  val exactRideSchedulerThreads    = rideSchedulerThreads.getOrElse(availableProcessors * 2).min(4)
  val grpcConnectorExecutorThreads = grpcApiMaxConcurrentRequests.fold(availableProcessors * 2)(_ + 1) // +1 for Blockchain Updates
}

case class WavesPublicApiSettings(
    restApi: String,
    grpcApi: String,
    grpcBlockchainUpdatesApi: String,
    noDataTimeout: FiniteDuration
)

case class RideRunnerResponseCacheSettings(
    size: ConfigMemorySize,
    ttl: FiniteDuration,
    gcThreshold: Int
)
