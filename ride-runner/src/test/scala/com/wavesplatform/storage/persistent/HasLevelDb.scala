package com.wavesplatform.storage.persistent

import com.google.common.io.MoreFiles
import com.wavesplatform.database.LevelDBFactory
import org.iq80.leveldb.{DB, Options}

import java.nio.file.Files

trait HasLevelDb {
  protected def withDb[A](f: DB => A): A = {
    val path = Files.createTempDirectory("lvl-temp").toAbsolutePath
    val db   = LevelDBFactory.factory.open(path.toFile, new Options().createIfMissing(true))
    try {
      f(db)
    } finally {
      db.close()
      MoreFiles.deleteRecursively(path)
    }
  }
}
