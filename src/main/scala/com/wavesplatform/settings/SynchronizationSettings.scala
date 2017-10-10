package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.network.MicroBlockSynchronizer
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.duration.FiniteDuration

case class SynchronizationSettings(maxRollback: Int,
                                   maxChainLength: Int,
                                   synchronizationTimeout: FiniteDuration,
                                   scoreTTL: FiniteDuration,
                                   microBlockSynchronizer: MicroBlockSynchronizer.Settings)

object SynchronizationSettings {
  val configPath: String = "waves.synchronization"

  def fromConfig(config: Config): SynchronizationSettings = {
    val maxRollback = config.as[Int](s"$configPath.max-rollback")
    val maxChainLength = config.as[Int](s"$configPath.max-chain-length")
    val synchronizationTimeout = config.as[FiniteDuration](s"$configPath.synchronization-timeout")
    val scoreTTL = config.as[FiniteDuration](s"$configPath.score-ttl")
    val microBlockSynchronizer = config.as[MicroBlockSynchronizer.Settings](s"$configPath.micro-block-synchronizer")

    SynchronizationSettings(maxRollback, maxChainLength, synchronizationTimeout, scoreTTL, microBlockSynchronizer)
  }
}