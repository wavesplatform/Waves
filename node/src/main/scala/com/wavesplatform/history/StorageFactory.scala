package com.wavesplatform.history

import com.wavesplatform.account.Address
import com.wavesplatform.database.{DBExt, Keys, RocksDBWriter, loadActiveLeases}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.mining.Miner
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockchainUpdaterImpl
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.{ScorexLogging, Time, UnsupportedFeature, forceStopApplication}
import monix.reactive.Observer
import org.rocksdb.RocksDB

object StorageFactory extends ScorexLogging {
  private val StorageVersion = 5

  def apply(
      settings: WavesSettings,
      db: RocksDB,
      time: Time,
      spendableBalanceChanged: Observer[(Address, Asset)],
      blockchainUpdateTriggers: BlockchainUpdateTriggers,
      miner: Miner = _ => ()
  ): (BlockchainUpdaterImpl, RocksDBWriter & AutoCloseable) = {
    checkVersion(db)
    val levelDBWriter = RocksDBWriter(db, spendableBalanceChanged, settings)
    val bui = new BlockchainUpdaterImpl(
      levelDBWriter,
      spendableBalanceChanged,
      settings,
      time,
      blockchainUpdateTriggers,
      (minHeight, maxHeight) => loadActiveLeases(db, minHeight, maxHeight),
      miner
    )
    (bui, levelDBWriter)
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
