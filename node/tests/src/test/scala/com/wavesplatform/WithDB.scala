package com.wavesplatform

import com.wavesplatform.account.Address
import com.wavesplatform.database.TestDB
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.transaction.Asset
import monix.reactive.subjects.{PublishSubject, Subject}
import org.iq80.leveldb.DB
import org.scalatest.{BeforeAndAfterEach, Suite}

trait WithDB extends BeforeAndAfterEach {
  this: Suite =>

  private var currentDBInstance: DB = _

  protected val ignoreSpendableBalanceChanged: Subject[(Address, Asset), (Address, Asset)] = PublishSubject()

  protected val ignoreBlockchainUpdateTriggers: BlockchainUpdateTriggers = BlockchainUpdateTriggers.noop

  def db: DB = currentDBInstance

  override def beforeEach(): Unit = {
    currentDBInstance = new TestDB
    super.beforeEach()
  }

  override def afterEach(): Unit =
    try {
      super.afterEach()
      db.close()
    } finally {
    }
}
