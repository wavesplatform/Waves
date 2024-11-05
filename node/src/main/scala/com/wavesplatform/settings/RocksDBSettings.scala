package com.wavesplatform.settings

import com.typesafe.config.Config

case class RocksDBSettings(
    mainCacheSize: SizeInBytes,
    txCacheSize: SizeInBytes,
    txMetaCacheSize: SizeInBytes,
    txSnapshotCacheSize: SizeInBytes,
    apiCacheSize: SizeInBytes,
    writeBufferSize: SizeInBytes,
    enableStatistics: Boolean,
    allowMmapReads: Boolean,
    parallelism: Int,
    maxOpenFiles: Int
)

object RocksDBSettings {
  def fromConfig(config: Config): RocksDBSettings =
    RocksDBSettings(
      mainCacheSize = SizeInBytes(config.getBytes("main-cache-size").toLong),
      txCacheSize = SizeInBytes(config.getBytes("tx-cache-size").toLong),
      txMetaCacheSize = SizeInBytes(config.getBytes("tx-meta-cache-size").toLong),
      txSnapshotCacheSize = SizeInBytes(config.getBytes("tx-snapshot-cache-size").toLong),
      apiCacheSize = SizeInBytes(config.getBytes("api-cache-size").toLong),
      writeBufferSize = SizeInBytes(config.getBytes("write-buffer-size").toLong),
      enableStatistics = config.getBoolean("enable-statistics"),
      allowMmapReads = config.getBoolean("allow-mmap-reads"),
      parallelism = config.getInt("parallelism"),
      maxOpenFiles = config.getInt("max-open-files")
    )
}
