package com.wavesplatform.ride.runner.entrypoints.settings

import com.typesafe.config.ConfigMemorySize
import com.wavesplatform.api.GrpcChannelSettings
import com.wavesplatform.ride.runner.caches.InMemBlockchainDataCache
import com.wavesplatform.ride.runner.db.RideRocksDb

import scala.concurrent.duration.FiniteDuration

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
