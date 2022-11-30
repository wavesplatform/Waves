package com.wavesplatform.storage.persistent

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.storage.AccountAssetKey
import com.wavesplatform.transaction.{Asset, AssetIdLength}
import com.wavesplatform.wallet.Wallet

import java.nio.charset.StandardCharsets

class AccountBalancePersistentCacheTestSuite extends BaseTestSuite with HasLevelDb {
  private val alice = Wallet.generateNewAccount("test".getBytes(StandardCharsets.UTF_8), 0)

  private val cacheValue: RemoteData[Long]        = RemoteData.Cached(1L)
  private val defaultCacheValue: RemoteData[Long] = RemoteData.Cached(0L) // Equal to RemoteData.Absence for this case

  "AccountBalancePersistentCache" - {
    "with Waves" - tests(Asset.Waves)
    "with IssuedAsset" - tests(Asset.IssuedAsset(ByteStr(Array.fill[Byte](AssetIdLength)(0))))
  }

  private def tests(asset: Asset): Unit = {
    val cacheKey = (alice.publicKey.toAddress, asset)
    "set and get" - {
      "cached" - {
        "on the first height" in test { cache =>
          cache.set(8, cacheKey, cacheValue)
          cache.set(11, cacheKey, RemoteData.Absence)

          cache.get(8, cacheKey) shouldBe cacheValue
        }

        "before the max height" in test { cache =>
          cache.set(8, cacheKey, cacheValue)
          cache.set(11, cacheKey, RemoteData.Absence)

          cache.get(10, cacheKey) shouldBe cacheValue
        }

        "on the max height" in test { cache =>
          cache.set(9, cacheKey, RemoteData.Absence)
          cache.set(10, cacheKey, cacheValue)

          cache.get(10, cacheKey) shouldBe cacheValue
        }

        "after the max height" in test { cache =>
          cache.set(10, cacheKey, cacheValue)
          cache.get(11, cacheKey) shouldBe cacheValue
        }
      }

      "absence" - {
        "before the max height" in test { cache =>
          cache.set(8, cacheKey, RemoteData.Absence)
          cache.set(11, cacheKey, cacheValue)

          cache.get(10, cacheKey) shouldBe defaultCacheValue
        }

        "on the max height" in test { cache =>
          cache.set(9, cacheKey, cacheValue)
          cache.set(10, cacheKey, RemoteData.Absence)

          cache.get(10, cacheKey) shouldBe defaultCacheValue
        }

        "after the max height" in test { cache =>
          cache.set(9, cacheKey, cacheValue)
          cache.set(10, cacheKey, RemoteData.Absence)

          cache.get(11, cacheKey) shouldBe defaultCacheValue
        }
      }

      "unknown" - {
        "on empty" in test { cache =>
          cache.get(10, cacheKey) shouldBe RemoteData.Unknown
        }

        "before the first known height" in test { cache =>
          cache.set(11, cacheKey, RemoteData.Absence)
          cache.get(10, cacheKey) shouldBe RemoteData.Unknown
        }
      }
    }

    "remove" - {
      "the data is not available for 'get' after deletion" - {
        "on removed height" in test { cache =>
          cache.set(9, cacheKey, RemoteData.Absence)
          cache.remove(9, cacheKey)

          cache.get(10, cacheKey) shouldBe RemoteData.Unknown
        }

        "on next height" in test { cache =>
          cache.set(11, cacheKey, RemoteData.Absence)
          cache.remove(1, cacheKey)

          cache.get(11, cacheKey) shouldBe RemoteData.Unknown
        }
      }

      "returns the last known value before deleted heights" - {
        "cached" in test { cache =>
          cache.set(9, cacheKey, cacheValue)
          cache.set(11, cacheKey, RemoteData.Absence)
          cache.remove(10, cacheKey)

          cache.get(10, cacheKey) shouldBe cacheValue
        }

        "absence" in test { cache =>
          cache.set(9, cacheKey, RemoteData.Absence)
          cache.set(11, cacheKey, cacheValue)
          cache.remove(10, cacheKey)

          cache.get(10, cacheKey) shouldBe defaultCacheValue
        }

        "unknown if empty" in test { cache =>
          cache.set(10, cacheKey, RemoteData.Absence)
          cache.set(11, cacheKey, cacheValue)
          cache.remove(10, cacheKey)

          cache.get(10, cacheKey) shouldBe RemoteData.Unknown
        }
      }
    }
  }

  private def test(f: PersistentCache[AccountAssetKey, Long] => Unit): Unit = withDb { db =>
    val caches = new LevelDbPersistentCaches(db)
    f(caches.accountBalances)
  }
}
