package com.wavesplatform.history

import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.BlockchainUpdaterImpl
import com.wavesplatform.state2.reader.SnapshotStateReader
import org.iq80.leveldb.DB
import scorex.transaction._
import scorex.utils.Time

object StorageFactory {

  def apply(settings: WavesSettings, db: DB, time: Time): (NgHistory with DebugNgHistory, SnapshotStateReader, BlockchainUpdaterImpl) = {
    val stateWriter = new LevelDBWriter(db, settings.blockchainSettings.functionalitySettings)
    val bcu = new BlockchainUpdaterImpl(stateWriter, settings, time, stateWriter)
    (bcu.historyReader, bcu.stateReader, bcu)
  }
}
