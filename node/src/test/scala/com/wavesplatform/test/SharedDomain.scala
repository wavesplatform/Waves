package com.wavesplatform.test

import java.nio.file.Files

import com.wavesplatform.database.{RDB, TestStorageFactory}
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.{NTPTime, TestHelpers}
import org.scalatest.{BeforeAndAfterAll, Suite}

trait SharedDomain extends BeforeAndAfterAll with NTPTime with DBCacheSettings { _: Suite =>
  private val path       = Files.createTempDirectory("rocks-temp").toAbsolutePath
  private val rdb        = RDB.open(dbSettings.copy(directory = path.toAbsolutePath.toString))
  private val (bui, ldb) = TestStorageFactory(settings, rdb, ntpTime, BlockchainUpdateTriggers.noop)

  def settings: WavesSettings               = DomainPresets.ScriptsAndSponsorship
  def genesisBalances: Seq[AddrWithBalance] = Seq.empty

  lazy val domain: Domain = Domain(rdb, bui, ldb, settings)

  override protected def beforeAll(): Unit = {
    val genesisTransactions = genesisBalances.map(ab => TxHelpers.genesis(ab.address, ab.balance))
    if (genesisTransactions.nonEmpty) {
      domain.appendBlock(genesisTransactions*)
    }
    super.beforeAll()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    bui.shutdown()
    ldb.close()
    rdb.close()
    TestHelpers.deleteRecursively(path)
  }
}
