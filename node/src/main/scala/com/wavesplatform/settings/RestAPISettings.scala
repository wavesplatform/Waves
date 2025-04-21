package com.wavesplatform.settings

import pureconfig.*

case class RestAPISettings(
    enable: Boolean,
    bindAddress: String,
    port: Int,
    apiKeyHash: String,
    corsHeaders: CorsHeaders,
    transactionsByAddressLimit: Int,
    transactionSnapshotsLimit: Int,
    distributionAddressLimit: Int,
    dataKeysRequestLimit: Int,
    assetDetailsLimit: Int,
    blocksRequestLimit: Int,
    evaluateScriptComplexityLimit: Int,
    limitedPoolThreads: Int,
    heavyRequestProcessorPoolThreads: Option[Int],
    minimumPeers: Int
) derives ConfigReader
