package com.wavesplatform.test

import com.wavesplatform.{NTPTime, TestHelpers}
import com.wavesplatform.database.{RDB, RocksDBWriter, TestStorageFactory}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.{DBCacheSettings, TempRDB}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockchainUpdaterImpl
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.{Args, BeforeAndAfterAll, OneInstancePerTest, Status, Suite}

trait SharedDomain extends BeforeAndAfterAll with ClosesAfterRun with NTPTime with DBCacheSettings { suite: Suite =>
  def settings: WavesSettings               = DomainPresets.ScriptsAndSponsorship
  def genesisBalances: Seq[AddrWithBalance] = Seq.empty

  private val storage = OnFirstUse {
    val db = TempRDB(s"rocks-temp-${getClass.getSimpleName}", dbSettings)
    try {
      val (bui, ldb) = openStorage(db.rdb)
      SharedDomain.Storage(db, bui, ldb)
    } catch {
      case e: Throwable =>
        db.close()
        throw e
    }
  }

  protected def openStorage(rdb: RDB): (BlockchainUpdaterImpl, RocksDBWriter) = TestStorageFactory(settings, rdb, ntpTime, BlockchainUpdateTriggers.noop)

  lazy val domain: Domain = Domain(storage.get.db.rdb, storage.get.bui, storage.get.ldb, settings)

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
    super.run(testName, args)
  }

  override protected def closeAfterRun(): Unit = TestHelpers.closeAll(() => super.closeAfterRun(), () => storage.close())
}

object SharedDomain {
  private final case class Storage(db: TempRDB, bui: BlockchainUpdaterImpl, ldb: RocksDBWriter) extends AutoCloseable {
    override def close(): Unit = TestHelpers.closeAll(() => bui.shutdown(), () => ldb.close(), () => db.close())
  }
}
