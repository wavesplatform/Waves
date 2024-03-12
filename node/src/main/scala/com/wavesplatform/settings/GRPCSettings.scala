package com.wavesplatform.settings

final case class GRPCSettings(
    host: String,
    port: Int,
    workerThreads: Int
)
