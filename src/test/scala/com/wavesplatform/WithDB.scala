package com.wavesplatform

import java.io.File
import java.nio.file.{Files, Path => JavaPath}

import com.wavesplatform.db.LevelDBFactory
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{Outcome, fixture}

import scala.reflect.io.Path


trait WithDB {
  this: fixture.TestSuite =>

  override type FixtureParam = DB

  override protected def withFixture(test: OneArgTest): Outcome = {
    val dir = Files.createTempDirectory("lvl").toAbsolutePath.toString
    val path = Path(dir)
    val options = new Options()
    options.createIfMissing(true)
    val db: DB = LevelDBFactory.factory.open(new File(dir), options)
    try {
      this.withFixture(test.toNoArgTest(db))
    } finally {
      db.close()
      path.deleteRecursively()
    }
  }
}

trait TestDB {

  def open(path: JavaPath): DB = {
    val options = new Options()
    options.createIfMissing(true)
    val file = new File(path.toAbsolutePath.toString)
    LevelDBFactory.factory.open(file, options)
  }

  def open(): DB = open(Files.createTempDirectory("lvl"))
}
