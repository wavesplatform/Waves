package com.wavesplatform.db

import com.wavesplatform.TestHelpers
import com.wavesplatform.database.{RDB, RocksDBWriter}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.state.BlockchainUpdaterImpl
import com.wavesplatform.test.{FreeSpec, FunSuite, SharedDomain}
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.{Args, DoNotDiscover, ParallelTestExecution, Reporter}

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*
import scala.util.Using

class TempDbCleanupSpec extends FreeSpec with DBCacheSettings {
  import TempDbCleanupSpec.*

  private def tempDirs(prefix: String): Set[Path] =
    Using.resource(Files.list(Paths.get(System.getProperty("java.io.tmpdir"))))(
      _.toScala(Set).filter(_.getFileName.toString.startsWith(prefix))
    )

  /** Temporary directories with `prefix` that running `suite` left behind. */
  private def leftAfterRunning(suite: org.scalatest.Suite, prefix: String): Set[Path] = {
    val before = tempDirs(prefix)
    val status = suite.run(None, Args(reporter = (_ => ()): Reporter))
    status.waitUntilCompleted()
    status.succeeds() shouldBe true
    tempDirs(prefix) -- before
  }

  // Holds when sbt forks the suite (build.sbt sets the property); an IDE runner that does not fork sees the system default
  "sbt forks node-tests with java.io.tmpdir in node-tests/test-tmp under target, which every test run empties first" in {
    val tmp      = Paths.get(System.getProperty("java.io.tmpdir")).toAbsolutePath
    val segments = tmp.iterator().asScala.map(_.toString).toSeq
    segments.takeRight(2) shouldBe Seq("node-tests", "test-tmp")
    segments should contain("target")
    Files.isDirectory(tmp) shouldBe true
  }

  "closeAll" - {
    "runs every step when earlier ones throw, and rethrows the first failure with the rest suppressed" in {
      val ran    = collection.mutable.Buffer.empty[Int]
      val first  = new IllegalStateException("first")
      val second = new IllegalStateException("second")
      val thrown = the[IllegalStateException] thrownBy TestHelpers.closeAll(
        () => { ran += 1; throw first },
        () => { ran += 2; throw second },
        () => ran += 3
      )
      ran shouldBe Seq(1, 2, 3)
      thrown shouldBe theSameInstanceAs(first)
      thrown.getSuppressed.toSeq shouldBe Seq(second)
    }
  }

  "TempRDB" - {
    "deletes its directory when the DB cannot be opened" in {
      val before = tempDirs("rocks-temp-FailingOpen")
      an[IllegalStateException] should be thrownBy TempRDB("rocks-temp-FailingOpen", dbSettings, _ => throw new IllegalStateException("open failed"))
      tempDirs("rocks-temp-FailingOpen") -- before shouldBe empty
    }
  }

  "SharedDomain" - {
    "deletes its DB when the suite completes" in {
      leftAfterRunning(new SharedDomainProbe, "rocks-temp-SharedDomainProbe") shouldBe empty
    }

    "deletes its DB when opening the storage on top of it fails" in {
      val before = tempDirs("rocks-temp-FailingStorageProbe")
      the[IllegalStateException] thrownBy {
        new FailingStorageProbe().run(None, Args(reporter = (_ => ()): Reporter))
      } should have message "storage failed"
      tempDirs("rocks-temp-FailingStorageProbe") -- before shouldBe empty
    }

    "refuses to run with an instance per test, leaving no DB behind" in {
      val before = tempDirs("rocks-temp-ParallelSharedDomainProbe")
      the[IllegalArgumentException] thrownBy {
        new ParallelSharedDomainProbe().run(None, Args(reporter = (_ => ()): Reporter))
      } should have message "requirement failed: SharedDomain shares one domain between tests, so it cannot run with an instance per test"
      tempDirs("rocks-temp-ParallelSharedDomainProbe") -- before shouldBe empty
    }
  }

  "WithState" - {
    "deletes the DB of every per-test instance of a ParallelTestExecution suite" in {
      leftAfterRunning(new ParallelWithDomainProbe, "rocks-temp-ParallelWithDomainProbe") shouldBe empty
    }
  }
}

object TempDbCleanupSpec {
  @DoNotDiscover
  class ParallelWithDomainProbe extends FunSuite with WithDomain with ParallelTestExecution {
    test("first")(withDomain()(_ => ()))
    test("second")(withDomain()(_ => ()))
  }

  @DoNotDiscover
  class ParallelSharedDomainProbe extends FunSuite with SharedDomain with ParallelTestExecution {
    test("uses the domain")(domain.blockchain.height)
  }

  @DoNotDiscover
  class SharedDomainProbe extends FunSuite with SharedDomain {
    override def genesisBalances: Seq[AddrWithBalance] = Seq(AddrWithBalance(TxHelpers.defaultAddress))
    test("uses the domain")(domain.blockchain.height shouldBe 1)
  }

  @DoNotDiscover
  class FailingStorageProbe extends FunSuite with SharedDomain {
    override def genesisBalances: Seq[AddrWithBalance]                                    = Seq(AddrWithBalance(TxHelpers.defaultAddress))
    override protected def openStorage(rdb: RDB): (BlockchainUpdaterImpl, RocksDBWriter) = throw new IllegalStateException("storage failed")
    test("never runs")(())
  }
}
