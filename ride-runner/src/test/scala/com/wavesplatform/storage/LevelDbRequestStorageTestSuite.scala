package com.wavesplatform.storage

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.Json

import java.nio.charset.StandardCharsets

class LevelDbRequestStorageTestSuite extends BaseTestSuite with HasLevelDb {
  private val alice = Wallet.generateNewAccount("test".getBytes(StandardCharsets.UTF_8), 0)

  "LevelDbRequestStorageTestSuite" - {
    "adds" in test { s =>
      s.all() shouldBe empty
      s.append((alice.toAddress, Json.obj("foo" -> 1)))
      s.all() should have length(1)
    }
  }

  private def test(f: RequestsStorage => Unit): Unit = withDb { db => f(new LevelDbRequestsStorage(db)) }
}
