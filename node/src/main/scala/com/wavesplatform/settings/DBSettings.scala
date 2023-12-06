package com.wavesplatform.settings
import scala.concurrent.duration.FiniteDuration

case class DBSettings(
    directory: String,
    storeTransactionsByAddress: Boolean,
    storeInvokeScriptResults: Boolean,
    storeStateHashes: Boolean,
    maxCacheSize: Int,
    maxRollbackDepth: Int,
    deleteOldDataInterval: Int = 1000,
    rememberBlocks: FiniteDuration,
    useBloomFilter: Boolean,
    rocksdb: RocksDBSettings
)
