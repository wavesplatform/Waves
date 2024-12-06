package com.wavesplatform.settings

import pureconfig.*
import pureconfig.generic.derivation.default.*

final case class GRPCSettings(
    host: String,
    port: Int,
    workerThreads: Int
) derives ConfigReader
