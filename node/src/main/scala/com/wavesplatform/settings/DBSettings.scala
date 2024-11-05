package com.wavesplatform.settings

import com.typesafe.config.Config

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
)

object DBSettings {
  def fromConfig(config: Config): DBSettings = DBSettings(
    directory = config.getString("directory"),
    storeTransactionsByAddress = config.getBoolean("store-transactions-by-address"),
    storeLeaseStatesByAddress = config.getBoolean("store-lease-states-by-address"),
    storeInvokeScriptResults = config.getBoolean("store-invoke-script-results"),
    storeStateHashes = config.getBoolean("store-state-hashes"),
    maxCacheSize = config.getInt("max-cache-size"),
    maxRollbackDepth = config.getInt("max-rollback-depth"),
    cleanupInterval = if (config.hasPath("cleanup-interval")) {
      Option(config.getInt("cleanup-interval"))
    } else {
      None
    },
    rocksdb = RocksDBSettings.fromConfig(config.getConfig("rocksdb"))
  )
}
