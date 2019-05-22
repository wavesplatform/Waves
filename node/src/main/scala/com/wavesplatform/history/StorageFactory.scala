package com.wavesplatform.history

import com.wavesplatform.account.Address
import com.wavesplatform.database.{DBExt, Keys, LevelDBWriter}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{BlockchainUpdaterImpl, NG}
import com.wavesplatform.transaction.{Asset, BlockchainUpdater}
import com.wavesplatform.utils.{ScorexLogging, Time, UnsupportedFeature, forceStopApplication}
import monix.reactive.Observer
import org.iq80.leveldb.DB

object StorageFactory extends ScorexLogging {
  private val StorageVersion = 4

  def apply(settings: WavesSettings, db: DB, time: Time, spendableBalanceChanged: Observer[(Address, Asset)]): BlockchainUpdater with NG = {
    checkVersion(db)
    val levelDBWriter = new LevelDBWriter(db, spendableBalanceChanged, settings.blockchainSettings, settings.dbSettings)
    new BlockchainUpdaterImpl(levelDBWriter, spendableBalanceChanged, settings, time)
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
          s"Storage version $version is not compatible with expected version $StorageVersion! Please, rebuild node's state, use import or sync from scratch.")
        log.error("FOR THIS REASON THE NODE STOPPED AUTOMATICALLY")
        forceStopApplication(UnsupportedFeature)
      }
    }
  }
}
