package com.wavesplatform.settings

case class RestAPISettings(
    enable: Boolean,
    bindAddress: String,
    port: Int,
    apiKeyHash: String,
    corsHeaders: CorsHeaders,
    transactionsByAddressLimit: Int,
    transactionsSnapshotsLimit: Int,
    distributionAddressLimit: Int,
    dataKeysRequestLimit: Int,
    assetDetailsLimit: Int,
    blocksRequestLimit: Int,
    evaluateScriptComplexityLimit: Int,
    limitedPoolThreads: Int,
    heavyRequestProcessorPoolThreads: Option[Int],
    minimumPeers: Int
)
