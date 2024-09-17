package com.wavesplatform

import java.nio.file.Files

import com.wavesplatform.database.RDB
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.events.BlockchainUpdateTriggers
import org.scalatest.{BeforeAndAfterEach, Suite}

trait WithNewDBForEachTest extends BeforeAndAfterEach with DBCacheSettings {
  this: Suite =>

  private val path                   = Files.createTempDirectory(s"rocks-${getClass.getSimpleName}").toAbsolutePath
  private var currentDBInstance: RDB = _

  protected val ignoreBlockchainUpdateTriggers: BlockchainUpdateTriggers = BlockchainUpdateTriggers.noop

  def db: RDB = currentDBInstance

  override def beforeEach(): Unit = {
    currentDBInstance = RDB.open(dbSettings.copy(directory = path.toAbsolutePath.toString))
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
