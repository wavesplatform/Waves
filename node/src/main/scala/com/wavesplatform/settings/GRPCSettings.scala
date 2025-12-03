package com.wavesplatform.settings

import pureconfig.*

final case class GRPCSettings(
    host: String,
    port: Int,
    workerThreads: Int
) derives ConfigReader
