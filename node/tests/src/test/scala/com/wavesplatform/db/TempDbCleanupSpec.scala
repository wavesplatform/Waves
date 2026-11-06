package com.wavesplatform.db

import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.test.{FreeSpec, FunSuite, SharedDomain}
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.{Args, DoNotDiscover, ParallelTestExecution, Reporter}

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*
import scala.util.Using

class TempDbCleanupSpec extends FreeSpec with DBCacheSettings {
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

  "node-tests keep temporary files under the build's target, where a killed run's leftovers are wiped by the next one" in {
    val tmp = Paths.get(System.getProperty("java.io.tmpdir")).toAbsolutePath
    tmp.iterator().asScala.map(_.toString).toSeq should contain("target")
    Files.isDirectory(tmp) shouldBe true
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
