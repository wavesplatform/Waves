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
    maxRollbackDepth: Int,
    cleanupInterval: Option[Int],
    rocksdb: RocksDBSettings
)

object DBSettings {
  given ConfigReader[DBSettings] = deriveReader[DBSettings]
}
