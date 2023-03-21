package com.wavesplatform.ride.runner.storage.persistent

import com.google.common.io.MoreFiles
import com.wavesplatform.ride.runner.db.RideDb.newColumnFamilyOptions
import com.wavesplatform.ride.runner.storage.persistent.HasDb.TestDb
import org.rocksdb.{ColumnFamilyDescriptor, ColumnFamilyHandle, DBOptions, DbPath, RocksDB, Statistics}

import java.nio.file.{Files, Path}
import java.util
import scala.jdk.CollectionConverters.SeqHasAsJava

trait HasDb {
  protected def withDb[A](f: PersistentStorage => A): A = TestDb.mk().withDb(f)
}

object HasDb {
  // TODO TestStorage
  case class TestDb(path: Path, db: RocksDB, clean: Boolean = true) extends AutoCloseable {
    val storage = PersistentStorage.rocksDb(db)

    def withoutCleaning: TestDb = copy(clean = false)

    def withDb[A](f: PersistentStorage => A): A = {
      try f(storage)
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
            newColumnFamilyOptions(12.0, 16 << 10, 128 << 20, 32 << 20, 0.6)
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
