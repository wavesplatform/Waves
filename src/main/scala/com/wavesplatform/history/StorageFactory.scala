package com.wavesplatform.history

import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{BlockchainUpdaterImpl, NG}
import org.iq80.leveldb.DB
import scorex.transaction.BlockchainUpdater
import scorex.utils.Time

object StorageFactory {
  def apply(settings: WavesSettings, db: DB, time: Time): BlockchainUpdater with NG = {
    val levelDBWriter = new LevelDBWriter(db, settings.blockchainSettings.functionalitySettings, settings.maxCacheSize)
    new BlockchainUpdaterImpl(levelDBWriter, settings, time)
  }
}
