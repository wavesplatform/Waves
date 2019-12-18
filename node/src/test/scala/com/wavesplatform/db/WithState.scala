package com.wavesplatform.db

import java.nio.file.Files

import com.wavesplatform.account.Address
import com.wavesplatform.database.{LevelDBWriter, openDB}
import com.wavesplatform.events.BlockchainUpdated
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, WavesSettings, loadConfig}
import com.wavesplatform.state.BlockchainUpdaterImpl
import com.wavesplatform.state.utils.TestLevelDB
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.Implicits.SubjectOps
import com.wavesplatform.{NTPTime, TestHelpers}
import monix.reactive.Observer
import monix.reactive.subjects.Subject
import org.scalatest.Suite

trait WithState extends DBCacheSettings {
  protected val ignoreSpendableBalanceChanged: Subject[(Address, Asset), (Address, Asset)] = Subject.empty
  protected val ignoreBlockchainUpdated: Subject[BlockchainUpdated, BlockchainUpdated]     = Subject.empty
  protected def withLevelDBWriter[A](bs: BlockchainSettings)(test: LevelDBWriter => A): A = {
    val path = Files.createTempDirectory("leveldb-test")
    val db   = openDB(path.toAbsolutePath.toString)
    try test(new LevelDBWriter(db, Observer.stopped, bs, dbSettings))
    finally {
      db.close()
      TestHelpers.deleteRecursively(path)
    }
  }

  def withLevelDBWriter[A](fs: FunctionalitySettings)(test: LevelDBWriter => A): A =
    withLevelDBWriter(TestLevelDB.createTestBlockchainSettings(fs))(test)
}

trait WithDomain extends WithState with NTPTime { _: Suite =>
  def withDomain[A](settings: WavesSettings = WavesSettings.fromRootConfig(loadConfig(None)))(test: Domain => A): A =
    withLevelDBWriter(settings.blockchainSettings) { blockchain =>
      val bcu = new BlockchainUpdaterImpl(blockchain, Observer.stopped, settings, ntpTime, ignoreBlockchainUpdated)
      try test(Domain(bcu, blockchain))
      finally bcu.shutdown()
    }
}
