package com.wavesplatform.settings

import pureconfig.*

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
) derives ConfigReader
