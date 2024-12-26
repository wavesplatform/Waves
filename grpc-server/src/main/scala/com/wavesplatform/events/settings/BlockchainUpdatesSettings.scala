package com.wavesplatform.events.settings

import scala.concurrent.duration.FiniteDuration

case class BlockchainUpdatesSettings(
    grpcPort: Int,
    minKeepAlive: FiniteDuration,
    workerThreads: Int
)
