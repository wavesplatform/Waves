package com.wavesplatform.ride.runner.entrypoints.settings

import scala.concurrent.duration.FiniteDuration

case class WavesPublicApiSettings(
    restApi: String,
    grpcApi: String,
    grpcBlockchainUpdatesApi: String,
    noDataTimeout: FiniteDuration
)
