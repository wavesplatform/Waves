package com.wavesplatform.history

import com.wavesplatform.database.{DBExt, Keys, RDB, RocksDBWriter, loadActiveLeases}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.mining.Miner
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockchainUpdaterImpl
import com.wavesplatform.utils.{ScorexLogging, Time, UnsupportedFeature, forceStopApplication}
import org.rocksdb.RocksDB

object StorageFactory extends ScorexLogging {
  private val StorageVersion = 1

  def apply(
      settings: WavesSettings,
      rdb: RDB,
      time: Time,
      blockchainUpdateTriggers: BlockchainUpdateTriggers,
      miner: Miner = _ => ()
  ): (BlockchainUpdaterImpl, RocksDBWriter) = {
    checkVersion(rdb.db)
    val rocksDBWriter = RocksDBWriter(rdb, settings.blockchainSettings, settings.dbSettings, settings.enableLightMode)
    val bui = new BlockchainUpdaterImpl(
      rocksDBWriter,
      settings,
      time,
      blockchainUpdateTriggers,
      (minHeight, maxHeight) => loadActiveLeases(rdb, minHeight, maxHeight),
      miner
    )
    (bui, rocksDBWriter)
  }

  private def checkVersion(db: RocksDB): Unit = db.readWrite { rw =>
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
