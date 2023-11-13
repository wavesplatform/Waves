package com.wavesplatform.ride.runner.entrypoints.settings

import com.typesafe.config.ConfigMemorySize

import scala.concurrent.duration.FiniteDuration

case class RideRunnerResponseCacheSettings(
    size: ConfigMemorySize,
    ttl: FiniteDuration,
    gcThreshold: Int
)
