package com.wavesplatform.storage.persistent

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.state.{BooleanDataEntry, DataEntry}
import com.wavesplatform.storage.AccountDataKey
import com.wavesplatform.wallet.Wallet

import java.nio.charset.StandardCharsets

class AccountDataPersistentCacheTestSuite extends BaseTestSuite with HasLevelDb {
  private val alice = Wallet.generateNewAccount("test".getBytes(StandardCharsets.UTF_8), 0)

  private val defaultPairDataKey = "foo"
  private val defaultPair        = (alice.publicKey.toAddress, defaultPairDataKey)
  private val defaultValue       = RemoteData.Cached(BooleanDataEntry(defaultPairDataKey, value = true))

  "AccountDataPersistentCache" - {
    "set and get" - {
      "cached" - {
        "on the first height" in test { cache =>
          cache.set(8, defaultPair, defaultValue)
          cache.set(11, defaultPair, RemoteData.Absence)

          cache.get(8, defaultPair) shouldBe defaultValue
        }

        "before the max height" in test { cache =>
          cache.set(8, defaultPair, defaultValue)
          cache.set(11, defaultPair, RemoteData.Absence)

          cache.get(10, defaultPair) shouldBe defaultValue
        }

        "on the max height" in test { cache =>
          cache.set(9, defaultPair, RemoteData.Absence)
          cache.set(10, defaultPair, defaultValue)

          cache.get(10, defaultPair) shouldBe defaultValue
        }

        "after the max height" in test { cache =>
          cache.set(10, defaultPair, defaultValue)
          cache.get(11, defaultPair) shouldBe defaultValue
        }
      }

      "absence" - {
        "before the max height" in test { cache =>
          cache.set(8, defaultPair, RemoteData.Absence)
          cache.set(11, defaultPair, defaultValue)

          cache.get(10, defaultPair) shouldBe RemoteData.Absence
        }

        "on the max height" in test { cache =>
          cache.set(9, defaultPair, defaultValue)
          cache.set(10, defaultPair, RemoteData.Absence)

          cache.get(10, defaultPair) shouldBe RemoteData.Absence
        }

        "after the max height" in test { cache =>
          cache.set(9, defaultPair, defaultValue)
          cache.set(10, defaultPair, RemoteData.Absence)

          cache.get(11, defaultPair) shouldBe RemoteData.Absence
        }
      }

      "unknown" - {
        "on empty" in test { cache =>
          cache.get(10, defaultPair) shouldBe RemoteData.Unknown
        }

        "before the first known height" in test { cache =>
          cache.set(11, defaultPair, RemoteData.Absence)
          cache.get(10, defaultPair) shouldBe RemoteData.Unknown
        }
      }
    }

    "remove" - {
      "the data is not available for 'get' after deletion" - {
        "on removed height" in test { cache =>
          cache.set(9, defaultPair, RemoteData.Absence)
          cache.remove(9, defaultPair)

          cache.get(10, defaultPair) shouldBe RemoteData.Unknown
        }

        "on next height" in test { cache =>
          cache.set(11, defaultPair, RemoteData.Absence)
          cache.remove(1, defaultPair)

          cache.get(11, defaultPair) shouldBe RemoteData.Unknown
        }
      }

      "returns the last known value before deleted heights" - {
        "cached" in test { cache =>
          cache.set(9, defaultPair, defaultValue)
          cache.set(11, defaultPair, RemoteData.Absence)
          cache.remove(10, defaultPair)

          cache.get(10, defaultPair) shouldBe defaultValue
        }

        "absence" in test { cache =>
          cache.set(9, defaultPair, RemoteData.Absence)
          cache.set(11, defaultPair, defaultValue)
          cache.remove(10, defaultPair)

          cache.get(10, defaultPair) shouldBe RemoteData.Absence
        }

        "unknown if empty" in test { cache =>
          cache.set(10, defaultPair, RemoteData.Absence)
          cache.set(11, defaultPair, defaultValue)
          cache.remove(10, defaultPair)

          cache.get(10, defaultPair) shouldBe RemoteData.Unknown
        }
      }
    }
  }

  private def test(f: PersistentCache[AccountDataKey, DataEntry[?]] => Unit): Unit = withDb { db =>
    val caches = new LevelDbPersistentCaches(db)
    f(caches.accountDataEntries)
  }
}
