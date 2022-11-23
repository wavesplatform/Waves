package com.wavesplatform

import com.typesafe.config.ConfigFactory

import java.nio.file.Files
import com.wavesplatform.account.Address
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.transaction.Asset
import monix.reactive.subjects.{PublishSubject, Subject}
import org.rocksdb.RocksDB
import org.scalatest.{BeforeAndAfterEach, Suite}

trait WithDB extends BeforeAndAfterEach with DBCacheSettings {
  this: Suite =>

  private val path                       = Files.createTempDirectory("rocks").toAbsolutePath
  private var currentDBInstance: RocksDB = _

  protected val ignoreSpendableBalanceChanged: Subject[(Address, Asset), (Address, Asset)] = PublishSubject()

  protected val ignoreBlockchainUpdateTriggers: BlockchainUpdateTriggers = BlockchainUpdateTriggers.noop

  def db: RocksDB = currentDBInstance

  override def beforeEach(): Unit = {
    currentDBInstance = database.openDB(dbSettings.copy(directory = path.toAbsolutePath.toString))
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
