package com.wavesplatform.settings

case class RestAPISettings(
    enable: Boolean,
    bindAddress: String,
    port: Int,
    apiKeyHash: String,
    cors: Boolean,
    apiKeyDifferentHost: Boolean,
    transactionsByAddressLimit: Int,
    distributionAddressLimit: Int,
    evaluateScriptComplexityLimit: Int,
    limitedPoolThreads: Int,
    minimumPeers: Int
)
