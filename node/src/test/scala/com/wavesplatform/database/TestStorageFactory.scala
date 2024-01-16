package com.wavesplatform.database

import com.google.common.util.concurrent.MoreExecutors
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
    val rocksDBWriter: RocksDBWriter = RocksDBWriter(
      rdb,
      settings.blockchainSettings,
      settings.dbSettings,
      settings.enableLightMode,
      100,
      Some(MoreExecutors.newDirectExecutorService())
    )
    (
      new BlockchainUpdaterImpl(rocksDBWriter, settings, time, blockchainUpdateTriggers, loadActiveLeases(rdb, _, _)),
      rocksDBWriter
    )
  }
}
