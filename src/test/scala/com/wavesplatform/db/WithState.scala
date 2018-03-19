package com.wavesplatform.db


import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import com.typesafe.config.ConfigFactory
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.{FunctionalitySettings, WavesSettings, loadConfig}
import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateWriter}
import scorex.transaction.History
import scorex.utils.{ScorexLogging, TimeImpl}

trait WithState extends ScorexLogging {
  private def withState[A](fs: FunctionalitySettings)
                          (f: (SnapshotStateReader with History with StateWriter) => A): A = {
    val path = Files.createTempDirectory("leveldb-test")
    val db = openDB(path.toAbsolutePath.toString, 2048000)
    try f(new LevelDBWriter(db, fs))
    finally {
      db.close()
      Files.walkFileTree(path, new SimpleFileVisitor[Path] {
        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
          Option(exc).fold {
            Files.delete(dir)
            FileVisitResult.CONTINUE
          }(throw _)
        }

        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }
      })
    }
  }

  def withStateAndHistory(fs: FunctionalitySettings)
                         (test: StateWriter with SnapshotStateReader with History => Any): Unit = withState(fs)(test)

  def withDomain[A](settings: WavesSettings = WavesSettings.fromConfig(loadConfig(ConfigFactory.load())))
                   (test: Domain => A): A = {
    val time = new TimeImpl

    try withState(settings.blockchainSettings.functionalitySettings) { stateWriter =>
      val bcu = new BlockchainUpdaterImpl(stateWriter, settings, time, stateWriter)
      try test(Domain(bcu.historyReader, bcu.stateReader, bcu))
      finally bcu.shutdown()
    }
    finally {
      time.close()
    }
  }
}
