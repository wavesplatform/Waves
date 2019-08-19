package com.wavesplatform

import java.nio.file.Files

import com.wavesplatform.account.Address
import com.wavesplatform.database.LevelDBFactory
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.Implicits.SubjectOps
import monix.reactive.subjects.Subject
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{BeforeAndAfterEach, TestSuite}

trait WithDB extends BeforeAndAfterEach {
  this: TestSuite =>

  private val path                  = Files.createTempDirectory("lvl").toAbsolutePath
  private var currentDBInstance: DB = _

  def db: DB = currentDBInstance

  protected val ignoreSpendableBalanceChanged: Subject[(Address, Asset), (Address, Asset)] = Subject.empty

  override def beforeEach(): Unit = {
    currentDBInstance = LevelDBFactory.factory.open(path.toFile, new Options().createIfMissing(true))
    super.beforeEach()
  }

  override def afterEach(): Unit =
    try {
      super.afterEach()
      db.close()
    } finally {
      TestHelpers.deleteRecursively(path)
    }

  protected def tempDb(f: DB => Any): Any = {
    val path = Files.createTempDirectory("lvl-temp").toAbsolutePath
    val db   = LevelDBFactory.factory.open(path.toFile, new Options().createIfMissing(true))
    try {
      f(db)
    } finally {
      db.close()
      TestHelpers.deleteRecursively(path)
    }
  }
}
