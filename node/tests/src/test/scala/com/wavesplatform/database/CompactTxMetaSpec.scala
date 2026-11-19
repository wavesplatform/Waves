package com.wavesplatform.database

import com.wavesplatform.TestHelpers
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.settings.DBSettings
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.utils.*
import org.rocksdb.FlushOptions

import java.nio.file.{Files, Path}
import scala.util.Using

class CompactTxMetaSpec extends FreeSpec with DBCacheSettings {
  private val Batches      = 3
  private val KeysPerBatch = 10

  private def withDbSettings(f: DBSettings => Unit): Unit = {
    val path = Files.createTempDirectory("rocks-temp-compact-tx-meta").toAbsolutePath
    try f(dbSettings.copy(directory = path.toString))
    finally TestHelpers.deleteRecursively(path)
  }

  private def l0Files(rdb: RDB): Long = rdb.db.getProperty(rdb.txMetaHandle.handle, "rocksdb.num-files-at-level0").toLong

  "tx-meta L0 files are merged, data is preserved" in withDbSettings { settings =>
    Using.resource(RDB.open(settings)) { rdb =>
      val h = rdb.txMetaHandle.handle
      (1 to Batches).foreach { batch =>
        (1 to KeysPerBatch).foreach(i => rdb.db.put(h, s"key-$batch-$i".utf8Bytes, s"value-$batch-$i".utf8Bytes))
        Using.resource(new FlushOptions().setWaitForFlush(true))(rdb.db.flush(_, h))
      }
      l0Files(rdb) shouldBe Batches
    }

    RDB.compactTxMeta(settings)

    Using.resource(RDB.open(settings)) { rdb =>
      l0Files(rdb) shouldBe 0
      for (batch <- 1 to Batches; i <- 1 to KeysPerBatch)
        rdb.db.get(rdb.txMetaHandle.handle, s"key-$batch-$i".utf8Bytes) shouldBe s"value-$batch-$i".utf8Bytes
    }
  }

  "a missing database is not created" in withDbSettings { settings =>
    val missing = Path.of(settings.directory, "missing")
    intercept[IllegalStateException](RDB.compactTxMeta(settings.copy(directory = missing.toString)))
    Files.exists(missing) shouldBe false
  }
}
