package com.wavesplatform.storage

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.storage.HasLevelDb.TestDb
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.Json

import java.nio.charset.StandardCharsets

class LevelDbRequestStorageTestSuite extends BaseTestSuite with HasLevelDb {
  private val alice = Wallet.generateNewAccount("alice".getBytes(StandardCharsets.UTF_8), 0)
  private val bob   = Wallet.generateNewAccount("bob".getBytes(StandardCharsets.UTF_8), 0)

  "LevelDbRequestStorageTestSuite" - {
    "added entries are preserved during restarts" in {
      val entry1 = (alice.toAddress, Json.obj("foo" -> 1))
      val entry2 = (bob.toAddress, Json.obj("bar" -> 2))

      val testPath = TestDb.mkTempPath
      TestDb.mk(testPath).withoutCleaning.withDb { db =>
        val s = new LevelDbRequestsStorage(db)
        s.all() shouldBe empty
        s.append(entry1)
      }

      TestDb.mk(testPath).withoutCleaning.withDb { db =>
        val s   = new LevelDbRequestsStorage(db)
        val all = s.all()
        all should have length 1
        all should contain(entry1)

        s.append(entry2)
      }

      TestDb.mk(testPath).withDb { db =>
        val s   = new LevelDbRequestsStorage(db)
        val all = s.all()
        all should have length 2
        all should contain(entry1)
        all should contain(entry2)
      }
    }
  }
}
