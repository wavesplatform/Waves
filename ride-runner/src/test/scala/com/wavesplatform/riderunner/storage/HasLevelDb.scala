package com.wavesplatform.riderunner.storage

import com.google.common.io.MoreFiles
import com.wavesplatform.database.LevelDBFactory
import HasLevelDb.TestDb
import org.iq80.leveldb.{DB, Options}

import java.nio.file.{Files, Path}

trait HasLevelDb {
  protected def withDb[A](f: DB => A): A = TestDb.mk().withDb(f)
}

object HasLevelDb {
  case class TestDb(path: Path, db: DB, clean: Boolean = true) extends AutoCloseable {
    def withoutCleaning: TestDb = copy(clean = false)

    def withDb[A](f: DB => A): A = {
      try f(db)
      finally close()
    }

    override def close(): Unit = {
      db.close()
      if (clean) MoreFiles.deleteRecursively(path)
    }
  }

  object TestDb {
    def mk(): TestDb = mk(mkTempPath)

    def mk(path: Path): TestDb = {
      val db = LevelDBFactory.factory.open(path.toFile, new Options().createIfMissing(true))
      TestDb(path, db)
    }

    def mkTempPath: Path = Files.createTempDirectory("lvl-temp").toAbsolutePath
  }
}
