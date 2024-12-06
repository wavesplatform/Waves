package com.wavesplatform.events.settings

import scala.concurrent.duration.FiniteDuration
import pureconfig.*
import pureconfig.generic.derivation.default.*

case class BlockchainUpdatesSettings(
    grpcPort: Int,
    minKeepAlive: FiniteDuration,
    workerThreads: Int
) derives ConfigReader
