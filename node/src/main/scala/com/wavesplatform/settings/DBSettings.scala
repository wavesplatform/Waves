package com.wavesplatform.settings

import pureconfig.*

case class DBSettings(
    directory: String,
    storeTransactionsByAddress: Boolean,
    storeLeaseStatesByAddress: Boolean,
    storeInvokeScriptResults: Boolean,
    storeStateHashes: Boolean,
    maxCacheSize: Int,
    maxRollbackDepth: Int,
    cleanupInterval: Option[Int] = defaultCleanupInterval,
    rocksdb: RocksDBSettings
)

object DBSettings {
  // Note: This setup (default values + manual ConfigReader instance) 
  // is a workaround for `pureconfig-generic-scala3` (it doesn't support default values from case classes yet)
  val defaultCleanupInterval: Option[Int] = None

  given ConfigReader[DBSettings] = ConfigReader.fromCursor(cur =>
    for {
      objCur <- cur.asObjectCursor
      directory <- objCur.required[String]("directory")
      storeTransactionsByAddress <- objCur.required[Boolean]("store-transactions-by-address")
      storeLeaseStatesByAddress <- objCur.required[Boolean]("store-lease-states-by-address")
      storeInvokeScriptResults <- objCur.required[Boolean]("store-invoke-script-results")
      storeStateHashes <- objCur.required[Boolean]("store-state-hashes")
      maxCacheSize <- objCur.required[Int]("max-cache-size")
      maxRollbackDepth <- objCur.required[Int]("max-rollback-depth")
      cleanupInterval <- objCur.optionalWithDefault("cleanup-interval", defaultCleanupInterval)
      rocksdb <- objCur.required[RocksDBSettings]("rocksdb")
    } yield DBSettings(
      directory,
      storeTransactionsByAddress,
      storeLeaseStatesByAddress,
      storeInvokeScriptResults,
      storeStateHashes,
      maxCacheSize,
      maxRollbackDepth,
      cleanupInterval,
      rocksdb
    )
  )
}
