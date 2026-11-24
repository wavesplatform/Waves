package com.wavesplatform.settings

import pureconfig.*
import pureconfig.generic.semiauto.deriveReader

case class DBSettings(
    directory: String,
    storeTransactionsByAddress: Boolean,
    storeLeaseStatesByAddress: Boolean,
    storeInvokeScriptResults: Boolean,
    storeStateHashes: Boolean,
    maxCacheSize: Int,
    maxDataCacheSize: Int,
    maxRollbackDepth: Int,
    cleanupInterval: Option[Int] = None,
    txBloomFilterSize: Int,
    rocksdb: RocksDBSettings,
    updateIfPresent: Boolean = true
)

object DBSettings {
  // This given is required for default args to work.
  // Details: https://github.com/pureconfig/pureconfig/issues/1673
  // Note: the proposed approach with `extension` doesn't work.
  given ConfigReader[DBSettings] = deriveReader
}
