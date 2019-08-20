package com.wavesplatform.db

import java.nio.file.Files

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.Address
import com.wavesplatform.database.{LevelDBWriter, openDB}
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.{FunctionalitySettings, WavesSettings, loadConfig}
import com.wavesplatform.state.{BlockchainUpdated, BlockchainUpdaterImpl}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.Implicits.SubjectOps
import com.wavesplatform.{NTPTime, TestHelpers}
import monix.reactive.Observer
import monix.reactive.subjects.Subject
import org.scalatest.Suite

trait WithState extends WithDBSettings {
  protected val ignoreSpendableBalanceChanged: Subject[(Address, Asset), (Address, Asset)] = Subject.empty
  protected val ignoreBlockchainUpdated: Subject[BlockchainUpdated, BlockchainUpdated]     = Subject.empty
  protected def withLevelDBWriter[A](fs: FunctionalitySettings)(test: LevelDBWriter => A): A = {
    val path = Files.createTempDirectory("leveldb-test")
    val db   = openDB(path.toAbsolutePath.toString)
    try test(new LevelDBWriter(db, Observer.stopped, fs, dbSettings.copy(directory = path.toAbsolutePath.toString)))
    finally {
      db.close()
      TestHelpers.deleteRecursively(path)
    }
  }
}

trait WithDomain extends WithState with NTPTime { _: Suite =>
  def withDomain[A](settings: WavesSettings = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load())))(test: Domain => A): A =
    withLevelDBWriter(settings.blockchainSettings.functionalitySettings) { blockchain =>
      val bcu = new BlockchainUpdaterImpl(blockchain, Observer.stopped, settings, ntpTime, ignoreBlockchainUpdated)
      try test(Domain(bcu))
      finally bcu.shutdown()
    }
}
