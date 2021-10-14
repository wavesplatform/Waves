package com.wavesplatform

import java.nio.file.Files

import com.wavesplatform.database.LevelDBFactory
import com.wavesplatform.events.BlockchainUpdateTriggers
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{BeforeAndAfterEach, Suite}

trait WithDB extends BeforeAndAfterEach {
  this: Suite =>

  private val path                  = Files.createTempDirectory("lvl").toAbsolutePath
  private var currentDBInstance: DB = _

  protected val ignoreBlockchainUpdateTriggers: BlockchainUpdateTriggers = BlockchainUpdateTriggers.noop

  def db: DB = currentDBInstance

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
}
