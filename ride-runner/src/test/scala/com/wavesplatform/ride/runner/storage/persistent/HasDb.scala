package com.wavesplatform.ride.runner.storage.persistent

import com.google.common.io.{MoreFiles, RecursiveDeleteOption}
import com.typesafe.config.ConfigMemorySize
import com.wavesplatform.ride.runner.db.{RideDb, RideDbAccess, RideRocksDb}
import com.wavesplatform.ride.runner.storage.persistent.HasDb.mkTestDb

import java.nio.file.{Files, Path}
import scala.util.Using

trait HasDb {
  protected def withDb[A](f: RideDbAccess => A): A = Using.resource(mkTestDb())(db => f(db.access))
}

object HasDb {
  def mkTestDb(path: Path = mkTempPath): RideDb = {
    val db = RideRocksDb.open(
      RideRocksDb.Settings(
        directory = path.toAbsolutePath.toString,
        enableStatistics = false,
        defaultColumnFamily = RideRocksDb.ColumnFamilySettings(
          bitsPerKey = 12,
          blockSize = ConfigMemorySize.ofBytes(16 << 10),
          cacheCapacity = ConfigMemorySize.ofBytes(128 << 20),
          writeBufferSize = ConfigMemorySize.ofBytes(32 << 20),
          highPriPoolRatio = 0.6
        )
      )
    )
    new RideDb {
      override def access: RideDbAccess = db.access
      override def close(): Unit = {
        db.close()
        MoreFiles.deleteRecursively(path, RecursiveDeleteOption.ALLOW_INSECURE)
      }
    }
  }

  def mkTempPath: Path = Files.createTempDirectory("lvl-temp").toAbsolutePath
}
