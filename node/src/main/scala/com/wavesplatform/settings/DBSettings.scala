package com.wavesplatform.settings
import scala.concurrent.duration.FiniteDuration

case class DBSettings(
    directory: String,
    storeTransactionsByAddress: Boolean,
    storeLeaseStatesByAddress: Boolean,
    storeInvokeScriptResults: Boolean,
    storeStateHashes: Boolean,
    maxCacheSize: Int,
    maxRollbackDepth: Int,
    rememberBlocks: FiniteDuration,
    useBloomFilter: Boolean,
    rocksdb: RocksDBSettings
)
