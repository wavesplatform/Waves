package com.wavesplatform.history

import com.wavesplatform.database.{loadActiveLeases, DBExt, Keys, LevelDBWriter}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.mining.Miner
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockchainUpdaterImpl
import com.wavesplatform.utils.{forceStopApplication, ScorexLogging, Time, UnsupportedFeature}
import org.iq80.leveldb.DB

object StorageFactory extends ScorexLogging {
  private val StorageVersion = 5

  def apply(settings: WavesSettings, db: DB, time: Time, blockchainUpdateTriggers: BlockchainUpdateTriggers, miner: Miner = _ => ()): (BlockchainUpdaterImpl, LevelDBWriter with AutoCloseable) = {
    checkVersion(db)
    val levelDBWriter = LevelDBWriter(db, settings)
    val bui = new BlockchainUpdaterImpl(levelDBWriter, settings, time, blockchainUpdateTriggers, (minHeight, maxHeight) => loadActiveLeases(db, minHeight, maxHeight), miner)
    (bui, levelDBWriter)
  }

  private def checkVersion(db: DB): Unit = db.readWrite { rw =>
    val version = rw.get(Keys.version)
    val height  = rw.get(Keys.height)
    if (version != StorageVersion) {
      if (height == 0) {
        // The storage is empty, set current version
        rw.put(Keys.version, StorageVersion)
      } else {
        // Here we've detected that the storage is not empty and doesn't contain version
        log.error(
          s"Storage version $version is not compatible with expected version $StorageVersion! Please, rebuild node's state, use import or sync from scratch."
        )
        log.error("FOR THIS REASON THE NODE STOPPED AUTOMATICALLY")
        forceStopApplication(UnsupportedFeature)
      }
    }
  }
}
