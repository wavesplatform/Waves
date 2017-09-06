package com.wavesplatform.history

import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateWriter}
import org.iq80.leveldb.DB
import scorex.transaction._
import scorex.utils.Time

object StorageFactory {

  def apply(settings: WavesSettings, db: DB, time: Time): (NgHistory with DebugNgHistory, StateWriter with SnapshotStateReader, BlockchainUpdaterImpl) = {
    val stateWriter = new LevelDBWriter(db, settings.blockchainSettings.functionalitySettings)
    val bcu = new BlockchainUpdaterImpl(stateWriter, settings, time, stateWriter)
    val history: NgHistory with DebugNgHistory with FeatureProvider = bcu.historyReader
    (history, stateWriter, bcu)
  }
}
