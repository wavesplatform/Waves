package com.wavesplatform

import java.nio.file.{Files, Path}

import com.wavesplatform.db.LevelDBFactory
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{BeforeAndAfterEach, TestSuite}

trait WithDB extends BeforeAndAfterEach {
  this: TestSuite =>

  var (db: DB, path: Path) = createDB

  override def beforeEach(): Unit = {
    val (d, p) = createDB
    db = d
    path = p
    super.beforeEach()
  }

  override def afterEach(): Unit = {
    try {
      super.afterEach()
    } finally {
      db.close()
      TestHelpers.deleteRecursively(path)
    }
  }

  private def createDB: (DB, Path) = {
    val path = Files.createTempDirectory("lvl").toAbsolutePath
    val db   = LevelDBFactory.factory.open(path.toFile, new Options().createIfMissing(true))
    (db, path)
  }
}

object DBExtensions {

  implicit class ExtDB(val db: DB) extends AnyVal {
    def clear(): Unit = {
      val b = db.createWriteBatch()

      val it = db.iterator()
      it.seekToFirst()
      while (it.hasNext) {
        val key = it.next().getKey
        b.delete(key)
      }
      it.close()
      db.write(b)
    }
  }

}
