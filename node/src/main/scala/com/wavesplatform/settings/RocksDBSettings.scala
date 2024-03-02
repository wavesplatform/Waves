package com.wavesplatform.settings

case class RocksDBSettings(
    mainCacheSize: SizeInBytes,
    txCacheSize: SizeInBytes,
    txMetaCacheSize: SizeInBytes,
    txSnapshotCacheSize: SizeInBytes,
    apiCacheSize: SizeInBytes,
    writeBufferSize: SizeInBytes,
    enableStatistics: Boolean,
    allowMmapReads: Boolean,
    parallelism: Int
)
