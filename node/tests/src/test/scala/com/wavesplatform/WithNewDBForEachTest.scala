package com.wavesplatform

import com.wavesplatform.database.RDB
import com.wavesplatform.db.{DBCacheSettings, TempRDB}
import com.wavesplatform.events.BlockchainUpdateTriggers
import org.scalatest.{BeforeAndAfterEach, Suite}

trait WithNewDBForEachTest extends BeforeAndAfterEach with DBCacheSettings {
  this: Suite =>

  private var currentDB: TempRDB = compiletime.uninitialized

  protected val ignoreBlockchainUpdateTriggers: BlockchainUpdateTriggers = BlockchainUpdateTriggers.noop

  def db: RDB = currentDB.rdb

  override def beforeEach(): Unit = {
    currentDB = TempRDB(s"rocks-temp-${getClass.getSimpleName}", dbSettings)
    super.beforeEach()
  }

  override def afterEach(): Unit =
    try super.afterEach()
    finally currentDB.close()
}
