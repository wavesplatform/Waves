package com.wavesplatform.database

import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockchainUpdaterImpl
import com.wavesplatform.utils.Time

object TestStorageFactory {
  def apply(
      settings: WavesSettings,
      rdb: RDB,
      time: Time,
      blockchainUpdateTriggers: BlockchainUpdateTriggers
  ): (BlockchainUpdaterImpl, RocksDBWriter) = {
    val rocksDBWriter: RocksDBWriter = new RocksDBWriter(rdb, settings.blockchainSettings, settings.dbSettings, settings.enableLightMode, 100)
    (
      new BlockchainUpdaterImpl(rocksDBWriter, settings, time, blockchainUpdateTriggers, loadActiveLeases(rdb, _, _)),
      rocksDBWriter
    )
  }
}
