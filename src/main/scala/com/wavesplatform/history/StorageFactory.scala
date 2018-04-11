package com.wavesplatform.history

import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{BlockchainUpdaterImpl, NG}
import org.iq80.leveldb.DB
import scorex.utils.Time

object StorageFactory {

  def apply(settings: WavesSettings, db: DB, time: Time): (NG, BlockchainUpdaterImpl) = {
    val stateWriter = new LevelDBWriter(db, settings.blockchainSettings.functionalitySettings)
    val bcu         = new BlockchainUpdaterImpl(stateWriter, settings, time)
    (bcu.historyReader, bcu)
  }
}
