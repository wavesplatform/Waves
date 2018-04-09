package com.wavesplatform.history

import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.reader.SnapshotStateReader
import com.wavesplatform.state.{BlockchainUpdaterImpl, NG}
import org.iq80.leveldb.DB
import scorex.utils.Time

object StorageFactory {

  def apply(settings: WavesSettings, db: DB, time: Time): (NG, SnapshotStateReader, BlockchainUpdaterImpl) = {
    val stateWriter = new LevelDBWriter(db, settings.blockchainSettings.functionalitySettings)
    val bcu         = new BlockchainUpdaterImpl(stateWriter, settings, time, stateWriter)
    (bcu.historyReader, bcu.stateReader, bcu)
  }
}
