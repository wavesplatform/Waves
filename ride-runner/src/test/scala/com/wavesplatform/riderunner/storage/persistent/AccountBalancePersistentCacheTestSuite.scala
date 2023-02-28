package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.riderunner.storage.AccountAssetKey
import com.wavesplatform.transaction.{Asset, AssetIdLength}

class AccountBalancePersistentCacheTestSuite extends PersistentTestSuite {
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
          cache.setDefault(8, cacheKey, cacheValue)
          cache.setDefault(11, cacheKey, RemoteData.Absence)

          cache.getDefault(8, cacheKey) shouldBe cacheValue
        }

        "before the max height" in test { cache =>
          cache.setDefault(8, cacheKey, cacheValue)
          cache.setDefault(11, cacheKey, RemoteData.Absence)

          cache.getDefault(10, cacheKey) shouldBe cacheValue
        }

        "on the max height" in test { cache =>
          cache.setDefault(9, cacheKey, RemoteData.Absence)
          cache.setDefault(10, cacheKey, cacheValue)

          cache.getDefault(10, cacheKey) shouldBe cacheValue
        }

        "after the max height" in test { cache =>
          cache.setDefault(10, cacheKey, cacheValue)
          cache.getDefault(11, cacheKey) shouldBe cacheValue
        }
      }

      "absence" - {
        "before the max height" in test { cache =>
          cache.setDefault(8, cacheKey, RemoteData.Absence)
          cache.setDefault(11, cacheKey, cacheValue)

          cache.getDefault(10, cacheKey) shouldBe defaultCacheValue
        }

        "on the max height" in test { cache =>
          cache.setDefault(9, cacheKey, cacheValue)
          cache.setDefault(10, cacheKey, RemoteData.Absence)

          cache.getDefault(10, cacheKey) shouldBe defaultCacheValue
        }

        "after the max height" in test { cache =>
          cache.setDefault(9, cacheKey, cacheValue)
          cache.setDefault(10, cacheKey, RemoteData.Absence)

          cache.getDefault(11, cacheKey) shouldBe defaultCacheValue
        }
      }

      "unknown" - {
        "on empty" in test { cache =>
          cache.getDefault(10, cacheKey) shouldBe RemoteData.Unknown
        }

        "before the first known height" in test { cache =>
          cache.setDefault(11, cacheKey, RemoteData.Absence)
          cache.getDefault(10, cacheKey) shouldBe RemoteData.Unknown
        }
      }
    }

    "remove" - {
      "the data is not available for 'get' after deletion" - {
        "on removed height" in test { cache =>
          cache.setDefault(9, cacheKey, RemoteData.Absence)
          cache.remove(9, cacheKey)

          cache.getDefault(10, cacheKey) shouldBe RemoteData.Unknown
        }

        "on next height" in test { cache =>
          cache.setDefault(11, cacheKey, RemoteData.Absence)
          cache.remove(1, cacheKey)

          cache.getDefault(11, cacheKey) shouldBe RemoteData.Unknown
        }
      }

      "returns the last known value before deleted heights" - {
        "cached" in test { cache =>
          cache.setDefault(9, cacheKey, cacheValue)
          cache.setDefault(11, cacheKey, RemoteData.Absence)
          cache.remove(10, cacheKey)

          cache.getDefault(10, cacheKey) shouldBe cacheValue
        }

        "absence" in test { cache =>
          cache.setDefault(9, cacheKey, RemoteData.Absence)
          cache.setDefault(11, cacheKey, cacheValue)
          cache.remove(10, cacheKey)

          cache.getDefault(10, cacheKey) shouldBe defaultCacheValue
        }

        "unknown if empty" in test { cache =>
          cache.setDefault(10, cacheKey, RemoteData.Absence)
          cache.setDefault(11, cacheKey, cacheValue)
          cache.remove(10, cacheKey)

          cache.getDefault(10, cacheKey) shouldBe RemoteData.Unknown
        }
      }
    }
  }

  private def test(f: PersistentCache[AccountAssetKey, Long] => Unit): Unit = withDb { db =>
    val caches = new LevelDbPersistentCaches(db)
    f(caches.accountBalances)
  }
}
