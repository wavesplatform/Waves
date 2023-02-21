package com.wavesplatform.riderunner.storage

import com.google.common.io.MoreFiles
import com.wavesplatform.database.RDB.newColumnFamilyOptions
import com.wavesplatform.riderunner.storage.HasLevelDb.TestDb
import org.rocksdb.{ColumnFamilyDescriptor, ColumnFamilyHandle, DBOptions, DbPath, RocksDB, Statistics}

import java.nio.file.{Files, Path}
import java.util
import scala.jdk.CollectionConverters.SeqHasAsJava

trait HasLevelDb {
  protected def withDb[A](f: RocksDB => A): A = TestDb.mk().withDb(f)
}

object HasLevelDb {
  case class TestDb(path: Path, db: RocksDB, clean: Boolean = true) extends AutoCloseable {
    def withoutCleaning: TestDb = copy(clean = false)

    def withDb[A](f: RocksDB => A): A = {
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
      val options = new DBOptions()
        .setStatistics(new Statistics())
        .setCreateIfMissing(true)
        .setBytesPerSync(2 << 20)
        .setCreateMissingColumnFamilies(true)
        .setMaxOpenFiles(100)

      val handles = new util.ArrayList[ColumnFamilyHandle]()
      val db = RocksDB.open(
        options,
        path.toString,
        Seq(
          new ColumnFamilyDescriptor(
            RocksDB.DEFAULT_COLUMN_FAMILY,
            newColumnFamilyOptions(12.0, 16 << 10, 512 << 20, 0.6)
              .setCfPaths(Seq(new DbPath(path.resolve("default"), 0L)).asJava)
          )
        ).asJava,
        handles
      )
      TestDb(path, db)
    }

    def mkTempPath: Path = Files.createTempDirectory("lvl-temp").toAbsolutePath
  }
}
