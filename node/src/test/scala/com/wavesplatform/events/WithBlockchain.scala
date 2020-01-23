package com.wavesplatform.events

import java.nio.file.Files

import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.{NTPTime, TestHelpers, database}
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.{Blockchain, BlockchainUpdaterImpl}
import com.wavesplatform.transaction.BlockchainUpdater
import monix.reactive.Observer
import org.scalatest.{BeforeAndAfterEach, Suite}

trait WithBlockchain extends BeforeAndAfterEach with NTPTime { _: Suite =>
  protected def settings: WavesSettings = WavesSettings.fromRootConfig(loadConfig(None))

  private val path = Files.createTempDirectory("leveldb-test")
  private val db   = database.openDB(path.toAbsolutePath.toString)
  private val bcu: Blockchain with BlockchainUpdater = new BlockchainUpdaterImpl(
    new LevelDBWriter(db, Observer.stopped, settings.blockchainSettings, settings.dbSettings),
    Observer.stopped,
    settings,
    ntpTime,
    BlockchainUpdateTriggers.noop
  )

  protected def blockchain: Blockchain = bcu

  /**
    * Override this method to do some initialization actions with
    * the blockchain before it becomes read-only
    * @param blockchainUpdater a blockchain to add something to (genesis, some blocks, etc.)
    */
  protected def initBlockchain(blockchainUpdater: Blockchain with BlockchainUpdater): Unit = ()

  override protected def beforeAll(): Unit = {
    initBlockchain(bcu)
    super.beforeAll()
  }

  override def afterAll() {
    bcu.shutdown()
    db.close()
    TestHelpers.deleteRecursively(path)
    super.afterAll()
  }
}
