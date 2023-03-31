package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.runner.db.RideDbAccess
import com.wavesplatform.ride.runner.storage.{AccountAssetKey, RemoteData}
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
        "on the first height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(8, cacheKey, cacheValue)
          }

          db.readWrite { implicit ctx =>
            cache.set(11, cacheKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.get(8, cacheKey) shouldBe cacheValue
          }
        }

        "before the max height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(8, cacheKey, cacheValue)
          }

          db.readWrite { implicit ctx =>
            cache.set(11, cacheKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, cacheKey) shouldBe cacheValue
          }
        }

        "on the max height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(9, cacheKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.set(10, cacheKey, cacheValue)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, cacheKey) shouldBe cacheValue
          }
        }

        "after the max height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(10, cacheKey, cacheValue)
          }

          db.readWrite { implicit ctx =>
            cache.get(11, cacheKey) shouldBe cacheValue
          }
        }
      }

      "absence" - {
        "before the max height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(8, cacheKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.set(11, cacheKey, cacheValue)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, cacheKey) shouldBe defaultCacheValue
          }
        }

        "on the max height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(9, cacheKey, cacheValue)
          }

          db.readWrite { implicit ctx =>
            cache.set(10, cacheKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, cacheKey) shouldBe defaultCacheValue
          }
        }

        "after the max height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(9, cacheKey, cacheValue)
          }

          db.readWrite { implicit ctx =>
            cache.set(10, cacheKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.get(11, cacheKey) shouldBe defaultCacheValue
          }
        }
      }

      "unknown" - {
        "on empty" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.get(10, cacheKey) shouldBe RemoteData.Unknown
          }
        }

        "before the first known height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(11, cacheKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, cacheKey) shouldBe RemoteData.Unknown
          }
        }
      }
    }

    "remove" - {
      "the data is not available for 'get' after deletion" - {
        "on removed height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(9, cacheKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.remove(9, cacheKey)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, cacheKey) shouldBe RemoteData.Unknown
          }
        }

        "on next height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(11, cacheKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.remove(1, cacheKey)
          }

          db.readWrite { implicit ctx =>
            cache.get(11, cacheKey) shouldBe RemoteData.Unknown
          }
        }
      }

      "returns the last known value before deleted heights" - {
        "cached" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(9, cacheKey, cacheValue)
          }

          db.readWrite { implicit ctx =>
            cache.set(11, cacheKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.remove(10, cacheKey)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, cacheKey) shouldBe cacheValue
          }
        }

        "absence" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(9, cacheKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.set(11, cacheKey, cacheValue)
          }

          db.readWrite { implicit ctx =>
            cache.remove(10, cacheKey)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, cacheKey) shouldBe defaultCacheValue
          }
        }

        "unknown if empty" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(10, cacheKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.set(11, cacheKey, cacheValue)
          }

          db.readWrite { implicit ctx =>
            cache.remove(10, cacheKey)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, cacheKey) shouldBe RemoteData.Unknown
          }
        }
      }
    }
  }

  private def test(f: (RideDbAccess, PersistentCache[AccountAssetKey, Long]) => Unit): Unit = withDb { db =>
    val caches = db.readWrite(DefaultPersistentCaches(db)(_))
    f(db, caches.accountBalances)
  }
}
