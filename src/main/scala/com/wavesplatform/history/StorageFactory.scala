package com.wavesplatform.history

import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{BlockchainUpdaterImpl, NG}
import com.wavesplatform.utils.Time
import org.iq80.leveldb.DB
import com.wavesplatform.transaction.BlockchainUpdater

object StorageFactory {
  def apply(settings: WavesSettings, db: DB, time: Time): BlockchainUpdater with NG = {
    val levelDBWriter = new LevelDBWriter(db, settings.blockchainSettings.functionalitySettings, settings.maxCacheSize)
    new BlockchainUpdaterImpl(levelDBWriter, settings, time)
  }
}
