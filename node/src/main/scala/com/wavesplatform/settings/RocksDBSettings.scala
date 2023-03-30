package com.wavesplatform.settings

case class RocksDBSettings(mainCacheSize: SizeInBytes, txCacheSize: SizeInBytes, txMetaCacheSize: SizeInBytes, enableStatistics: Boolean)
