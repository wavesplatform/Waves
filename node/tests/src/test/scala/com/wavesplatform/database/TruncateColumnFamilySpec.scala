package com.wavesplatform.database

import com.wavesplatform.TestHelpers
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.settings.DBSettings
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.utils.*
import org.rocksdb.ColumnFamilyHandle

import java.nio.file.Files
import scala.util.Using

class TruncateColumnFamilySpec extends FreeSpec with DBCacheSettings {
  private def withDbSettings(f: DBSettings => Unit): Unit = {
    val path = Files.createTempDirectory("rocks-temp-truncate-cf").toAbsolutePath
    try f(dbSettings.copy(directory = path.toAbsolutePath.toString))
    finally TestHelpers.deleteRecursively(path)
  }

  private def handleOf(rdb: RDB, cfName: String): ColumnFamilyHandle = cfName match {
    case RDB.TxSnapshotCF => rdb.txSnapshotHandle.handle
    case RDB.ApiCF        => rdb.apiHandle.handle
  }

  private def put(rdb: RDB, handle: ColumnFamilyHandle, count: Int): Unit =
    (1 to count).foreach(i => rdb.db.put(handle, s"key-$i".utf8Bytes, s"value-$i".utf8Bytes))

  private def count(rdb: RDB, handle: ColumnFamilyHandle): Int =
    Using.resource(rdb.db.newIterator(handle)) { iter =>
      iter.seekToFirst()
      var result = 0
      while (iter.isValid) {
        result += 1
        iter.next()
      }
      result
    }

  RDB.NonEssentialColumnFamilies.foreach { cfName =>
    s"$cfName is truncated, other column families are left intact" in withDbSettings { settings =>
      Using.resource(RDB.open(settings)) { rdb =>
        put(rdb, rdb.db.getDefaultColumnFamily, 10)
        put(rdb, rdb.txHandle.handle, 10)
        RDB.NonEssentialColumnFamilies.foreach(name => put(rdb, handleOf(rdb, name), 10))
      }

      RDB.truncateColumnFamily(settings, cfName)

      Using.resource(RDB.open(settings)) { rdb =>
        count(rdb, handleOf(rdb, cfName)) shouldBe 0
        count(rdb, rdb.db.getDefaultColumnFamily) shouldBe 10
        count(rdb, rdb.txHandle.handle) shouldBe 10
        RDB.NonEssentialColumnFamilies.filterNot(_ == cfName).foreach(name => count(rdb, handleOf(rdb, name)) shouldBe 10)
      }
    }
  }

  "essential column family can not be truncated" in withDbSettings { settings =>
    Using.resource(RDB.open(settings))(_ => ())
    intercept[IllegalArgumentException](RDB.truncateColumnFamily(settings, RDB.TxCF))
  }
}
