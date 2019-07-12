package com.wavesplatform.db

import java.nio.file.Files

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.Address
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.{FunctionalitySettings, WavesSettings, loadConfig}
import com.wavesplatform.state.BlockchainUpdaterImpl
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.Implicits.SubjectOps
import com.wavesplatform.{NTPTime, TestHelpers}
import monix.reactive.subjects.Subject
import org.scalatest.Suite

trait WithState extends WithDBSettings {
  protected val ignoreSpendableBalanceChanged: Subject[(Address, Asset), (Address, Asset)] = Subject.empty
  protected def withState[A](fs: FunctionalitySettings)(f: LevelDBWriter => A): A = {
    val path = Files.createTempDirectory("leveldb-test")
    val db   = openDB(path.toAbsolutePath.toString)
    try f(new LevelDBWriter(db, ignoreSpendableBalanceChanged, fs, dbSettings.copy(directory = path.toAbsolutePath.toString)))
    finally {
      db.close()
      TestHelpers.deleteRecursively(path)
    }
  }

  def withStateAndHistory(fs: FunctionalitySettings)(test: LevelDBWriter => Any): Unit = withState(fs)(test)
}

trait WithDomain extends WithState with NTPTime {
  _: Suite =>

  def withDomain[A](settings: WavesSettings = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load())))(test: Domain => A): A = {
    try withState(settings.blockchainSettings.functionalitySettings) { blockchain =>
      val bcu = new BlockchainUpdaterImpl(blockchain, ignoreSpendableBalanceChanged, settings, ntpTime)
      try test(Domain(bcu))
      finally bcu.shutdown()
    } finally {}
  }
}
