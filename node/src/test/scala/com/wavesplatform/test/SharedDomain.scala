package com.wavesplatform.test

import java.nio.file.Files
import com.wavesplatform.{NTPTime, TestHelpers, database}
import com.wavesplatform.database.TestStorageFactory
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.transaction.TxHelpers
import monix.reactive.Observer
import org.rocksdb.RocksDB
import org.scalatest.{BeforeAndAfterAll, Suite}

trait SharedDomain extends BeforeAndAfterAll with NTPTime { _: Suite =>
  private val path        = Files.createTempDirectory("rocks-temp").toAbsolutePath
  private val db: RocksDB = database.openDB(path.toAbsolutePath.toString)
  private val (bui, ldb)  = TestStorageFactory(settings, db, ntpTime, Observer.stopped, BlockchainUpdateTriggers.noop)

  def settings: WavesSettings               = DomainPresets.ScriptsAndSponsorship
  def genesisBalances: Seq[AddrWithBalance] = Seq.empty

  lazy val domain: Domain = Domain(db, bui, ldb, settings)

  override protected def beforeAll(): Unit = {
    val genesisTransactions = genesisBalances.map(ab => TxHelpers.genesis(ab.address, ab.balance))
    if (genesisTransactions.nonEmpty) {
      domain.appendBlock(genesisTransactions*)
    }
    super.beforeAll()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    db.close()
    bui.shutdown()
    TestHelpers.deleteRecursively(path)
  }
}
