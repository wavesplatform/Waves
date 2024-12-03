package com.wavesplatform.settings

import pureconfig.*
import pureconfig.generic.derivation.default.*

case class DBSettings(
    directory: String,
    storeTransactionsByAddress: Boolean,
    storeLeaseStatesByAddress: Boolean,
    storeInvokeScriptResults: Boolean,
    storeStateHashes: Boolean,
    maxCacheSize: Int,
    maxRollbackDepth: Int,
    cleanupInterval: Option[Int] = None,
    rocksdb: RocksDBSettings
) derives ConfigReader
