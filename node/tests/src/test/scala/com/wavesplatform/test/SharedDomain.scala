package com.wavesplatform.test

import com.wavesplatform.NTPTime
import com.wavesplatform.database.{RocksDBWriter, TestStorageFactory}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.{DBCacheSettings, TempRDB}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockchainUpdaterImpl
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.{Args, BeforeAndAfterAll, OneInstancePerTest, Status, Suite}

trait SharedDomain extends BeforeAndAfterAll with NTPTime with DBCacheSettings { suite: Suite =>
  def settings: WavesSettings               = DomainPresets.ScriptsAndSponsorship
  def genesisBalances: Seq[AddrWithBalance] = Seq.empty

  // Opened on first use and closed when the suite's run completes, so nothing is left behind whichever step fails
  @volatile private var opened = false
  private lazy val storage = {
    val db = TempRDB(s"rocks-temp-${getClass.getSimpleName}", dbSettings)
    try {
      val (bui, ldb) = TestStorageFactory(settings, db.rdb, ntpTime, BlockchainUpdateTriggers.noop)
      opened = true
      SharedDomain.Storage(db, bui, ldb)
    } catch {
      case e: Throwable =>
        db.close()
        throw e
    }
  }

  lazy val domain: Domain = Domain(storage.db.rdb, storage.bui, storage.ldb, settings)

  override protected def beforeAll(): Unit = {
    val genesisTransactions = genesisBalances.map(ab => TxHelpers.genesis(ab.address, ab.balance))
    if (genesisTransactions.nonEmpty) {
      domain.appendBlock(genesisTransactions*)
    }
    super.beforeAll()
  }

  abstract override def run(testName: Option[String], args: Args): Status = {
    // Per-test instances never run beforeAll, so each test would get an empty domain of its own instead of the shared one
    require(!this.isInstanceOf[OneInstancePerTest], "SharedDomain shares one domain between tests, so it cannot run with an instance per test")
    val status =
      try super.run(testName, args)
      catch {
        case e: Throwable =>
          closeStorage()
          throw e
      }
    status.whenCompleted(_ => closeStorage())
    status
  }

  private def closeStorage(): Unit = if (opened) storage.close()
}

object SharedDomain {
  private final case class Storage(db: TempRDB, bui: BlockchainUpdaterImpl, ldb: RocksDBWriter) extends AutoCloseable {
    override def close(): Unit =
      try bui.shutdown()
      finally
        try ldb.close()
        finally db.close()
  }
}
