package com.wavesplatform.riderunner.storage

import com.wavesplatform.riderunner.storage.HasDb.TestDb
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}
import play.api.libs.json.Json

class LevelDbRequestStorageTestSuite extends BaseTestSuite with HasDb with HasTestAccounts {
  "LevelDbRequestStorageTestSuite" - {
    "added entries are preserved during restarts" in {
      val entry1 = ScriptRequest(alice.toAddress, Json.obj("foo" -> 1))
      val entry2 = ScriptRequest(bob.toAddress, Json.obj("bar" -> 2))

      val testPath = TestDb.mkTempPath
      TestDb.mk(testPath).withoutCleaning.withDb { db =>
        val s = new DefaultRequestsStorage(db)
        s.all() shouldBe empty
        s.append(entry1)
      }

      TestDb.mk(testPath).withoutCleaning.withDb { db =>
        val s   = new DefaultRequestsStorage(db)
        val all = s.all()
        all should have length 1
        all should contain(entry1)

        s.append(entry2)
      }

      TestDb.mk(testPath).withDb { db =>
        val s   = new DefaultRequestsStorage(db)
        val all = s.all()
        all should have length 2
        all should contain(entry1)
        all should contain(entry2)
      }
    }
  }
}
