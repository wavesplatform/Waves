package com.wavesplatform

import java.io.File
import java.nio.file.Files

import com.wavesplatform.db.LevelDBFactory
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{BeforeAndAfterEach, TestSuite}

import scala.reflect.io.Path

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
      path.deleteRecursively()
    }
  }

  private def createDB: (DB, Path) = {
    val dir     = Files.createTempDirectory("lvl").toAbsolutePath.toString
    val path    = Path(dir)
    val options = new Options()
    options.createIfMissing(true)
    val db = LevelDBFactory.factory.open(new File(dir), options)
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
