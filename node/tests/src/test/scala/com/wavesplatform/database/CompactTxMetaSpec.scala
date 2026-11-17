package com.wavesplatform.database

import com.wavesplatform.TestHelpers
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.utils.*
import org.rocksdb.FlushOptions

import java.nio.file.Files
import scala.util.Using

class CompactTxMetaSpec extends FreeSpec with DBCacheSettings {
  private val Batches      = 3
  private val KeysPerBatch = 10

  private def l0Files(rdb: RDB): Long = rdb.db.getProperty(rdb.txMetaHandle.handle, "rocksdb.num-files-at-level0").toLong

  "tx-meta L0 files are merged, data is preserved" in {
    val path     = Files.createTempDirectory("rocks-temp-compact-tx-meta").toAbsolutePath
    val settings = dbSettings.copy(directory = path.toString)
    try {
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
    } finally TestHelpers.deleteRecursively(path)
  }

  "a missing database is not created" in {
    val parent = Files.createTempDirectory("rocks-temp-compact-tx-meta").toAbsolutePath
    val path   = parent.resolve("missing")
    try {
      intercept[IllegalStateException](RDB.compactTxMeta(dbSettings.copy(directory = path.toString)))
      Files.exists(path) shouldBe false
    } finally TestHelpers.deleteRecursively(parent)
  }
}
