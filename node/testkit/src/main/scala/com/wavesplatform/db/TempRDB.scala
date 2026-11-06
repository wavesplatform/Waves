package com.wavesplatform.db

import com.wavesplatform.TestHelpers
import com.wavesplatform.database.RDB
import com.wavesplatform.settings.DBSettings

import java.nio.file.{Files, Path}

/** A RocksDB in a fresh temporary directory; closing it also deletes the directory. */
final class TempRDB private (val path: Path, val rdb: RDB) extends AutoCloseable {
  override def close(): Unit =
    try rdb.close()
    finally TestHelpers.deleteRecursively(path)
}

object TempRDB {
  def apply(prefix: String, settings: DBSettings): TempRDB = apply(prefix, settings, RDB.open)

  private[db] def apply(prefix: String, settings: DBSettings, open: DBSettings => RDB): TempRDB = {
    val path = Files.createTempDirectory(prefix).toAbsolutePath
    try new TempRDB(path, open(settings.copy(directory = path.toString)))
    catch {
      case e: Throwable =>
        TestHelpers.deleteRecursively(path)
        throw e
    }
  }
}
