package com.wavesplatform.ride.runner.entrypoints.settings

import com.typesafe.config.*
import com.wavesplatform.api.DefaultBlockchainApi
import com.wavesplatform.ride.runner.BlockchainState
import com.wavesplatform.ride.runner.caches.SharedBlockchainStorage
import com.wavesplatform.ride.runner.entrypoints.{Heights, WavesRideRunnerCompareService}
import com.wavesplatform.ride.runner.requests.DefaultRequestService
import com.wavesplatform.settings.*
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*

import scala.concurrent.duration.DurationInt

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
    memBlockchainDataCache = rideRunner.memBlockchainDataCache
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
}
