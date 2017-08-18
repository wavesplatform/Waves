package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.network.MircoBlockSynchronizer
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.duration.FiniteDuration

case class SynchronizationSettings(maxRollback: Int,
                                   maxChainLength: Int,
                                   loadEntireChain: Boolean,
                                   synchronizationTimeout: FiniteDuration,
                                   pinToInitialPeer: Boolean,
                                   retriesBeforeBlacklisting: Int,
                                   operationRetries: Int,
                                   scoreBroadcastInterval: FiniteDuration,
                                   scoreTTL: FiniteDuration,
                                   microBlockSynchronizer: MircoBlockSynchronizer.Settings)

object SynchronizationSettings {
  val configPath: String = "waves.synchronization"

  def fromConfig(config: Config): SynchronizationSettings = {
    val maxRollback = config.as[Int](s"$configPath.max-rollback")
    val maxChainLength = config.as[Int](s"$configPath.max-chain-length")
    val loadEntireChain = config.as[Boolean](s"$configPath.load-entire-chain")
    val synchronizationTimeout = config.as[FiniteDuration](s"$configPath.synchronization-timeout")
    val pinToInitialPeer = config.as[Boolean](s"$configPath.pin-to-initial-peer")
    val retriesBeforeBlacklisting = config.as[Int](s"$configPath.retries-before-blacklisting")
    val operationRetries = config.as[Int](s"$configPath.operation-retires")
    val scoreBroadcastInterval = config.as[FiniteDuration](s"$configPath.score-broadcast-interval")
    val scoreTTL = config.as[FiniteDuration](s"$configPath.score-ttl")
    val microBlockSynchronizer = config.as[MircoBlockSynchronizer.Settings](s"$configPath.micro-block-synchronizer")

    SynchronizationSettings(maxRollback, maxChainLength, loadEntireChain, synchronizationTimeout, pinToInitialPeer,
      retriesBeforeBlacklisting, operationRetries, scoreBroadcastInterval, scoreTTL, microBlockSynchronizer)
  }
}