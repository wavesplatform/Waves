package com.wavesplatform.settings

case class RocksDBSettings(
    mainCacheSize: SizeInBytes,
    txCacheSize: SizeInBytes,
    txMetaCacheSize: SizeInBytes,
    txSnapshotCacheSize: SizeInBytes,
    writeBufferSize: SizeInBytes,
    enableStatistics: Boolean
)
