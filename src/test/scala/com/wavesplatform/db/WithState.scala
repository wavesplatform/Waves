package com.wavesplatform.db

import java.nio.file.Files

import com.typesafe.config.ConfigFactory
import com.wavesplatform.TestHelpers
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.{FunctionalitySettings, WavesSettings, loadConfig}
import com.wavesplatform.state.{Blockchain, BlockchainUpdaterImpl}
import com.wavesplatform.utils.{ScorexLogging, TimeImpl}

trait WithState extends ScorexLogging {
  private def withState[A](fs: FunctionalitySettings)(f: Blockchain => A): A = {
    val path = Files.createTempDirectory("leveldb-test")
    val db   = openDB(path.toAbsolutePath.toString)
    try f(new LevelDBWriter(db, fs))
    finally {
      db.close()
      TestHelpers.deleteRecursively(path)
    }
  }

  def withStateAndHistory(fs: FunctionalitySettings)(test: Blockchain => Any): Unit = withState(fs)(test)

  def withDomain[A](settings: WavesSettings = WavesSettings.fromConfig(loadConfig(ConfigFactory.load())))(test: Domain => A): A = {
    val time = new TimeImpl

    try withState(settings.blockchainSettings.functionalitySettings) { blockchain =>
      val bcu = new BlockchainUpdaterImpl(blockchain, settings, time)
      try test(Domain(bcu))
      finally bcu.shutdown()
    } finally {
      time.close()
    }
  }
}
