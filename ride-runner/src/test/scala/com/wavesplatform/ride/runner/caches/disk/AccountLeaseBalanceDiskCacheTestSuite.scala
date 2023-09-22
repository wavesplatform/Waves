package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.account.Address
import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.db.RideDbAccess
import com.wavesplatform.state.{Height, LeaseBalance}

class AccountLeaseBalanceDiskCacheTestSuite extends DiskTestSuite {
  private val cacheKey                                    = aliceAddr
  private val cacheValue: RemoteData[LeaseBalance]        = RemoteData.Cached(LeaseBalance(12L, 14L))
  private val defaultCacheValue: RemoteData[LeaseBalance] = RemoteData.Cached(LeaseBalance.empty) // Equal to RemoteData.Absence for this case

  "AccountLeaseBalanceDiskCache" - {
    "set and get" - {
      "cached" - {
        "on the first height" in test { (db, cache) =>
          db.directReadWrite { implicit ctx =>
            cache.set(Height(8), cacheKey, cacheValue)
            cache.set(Height(11), cacheKey, RemoteData.Absence)
            cache.get(Height(8), cacheKey) shouldBe cacheValue
          }
        }

        "before the max height" in test { (db, cache) =>
          db.directReadWrite { implicit ctx =>
            cache.set(Height(8), cacheKey, cacheValue)
            cache.set(Height(11), cacheKey, RemoteData.Absence)
            cache.get(Height(10), cacheKey) shouldBe cacheValue
          }
        }

        "on the max height" in test { (db, cache) =>
          db.directReadWrite { implicit ctx =>
            cache.set(Height(9), cacheKey, RemoteData.Absence)
            cache.set(Height(10), cacheKey, cacheValue)
            cache.get(Height(10), cacheKey) shouldBe cacheValue
          }
        }

        "after the max height" in test { (db, cache) =>
          db.directReadWrite { implicit ctx =>
            cache.set(Height(10), cacheKey, cacheValue)
            cache.get(Height(11), cacheKey) shouldBe cacheValue
          }
        }
      }

      "absence" - {
        "before the max height" in test { (db, cache) =>
          db.directReadWrite { implicit ctx =>
            cache.set(Height(8), cacheKey, RemoteData.Absence)
            cache.set(Height(11), cacheKey, cacheValue)
            cache.get(Height(10), cacheKey) shouldBe defaultCacheValue
          }
        }

        "on the max height" in test { (db, cache) =>
          db.directReadWrite { implicit ctx =>
            cache.set(Height(9), cacheKey, cacheValue)
            cache.set(Height(10), cacheKey, RemoteData.Absence)
            cache.get(Height(10), cacheKey) shouldBe defaultCacheValue
          }
        }

        "after the max height" in test { (db, cache) =>
          db.directReadWrite { implicit ctx =>
            cache.set(Height(9), cacheKey, cacheValue)
            cache.set(Height(10), cacheKey, RemoteData.Absence)
            cache.get(Height(11), cacheKey) shouldBe defaultCacheValue
          }
        }
      }

      "unknown" - {
        "on empty" in test { (db, cache) =>
          db.directReadWrite { implicit ctx =>
            cache.get(Height(10), cacheKey) shouldBe RemoteData.Unknown
          }
        }

        "before the first known height" in test { (db, cache) =>
          db.directReadWrite { implicit ctx =>
            cache.set(Height(11), cacheKey, RemoteData.Absence)
            cache.get(Height(10), cacheKey) shouldBe RemoteData.Unknown
          }
        }
      }
    }

    "remove" - {
      "the data is not available for 'get' after deletion" - {
        "on removed height" in test { (db, cache) =>
          db.directReadWrite { implicit ctx =>
            cache.set(Height(9), cacheKey, RemoteData.Absence)
            cache.removeFrom(Height(9), cacheKey)
            cache.get(Height(10), cacheKey) shouldBe RemoteData.Unknown
          }
        }

        "on next height" in test { (db, cache) =>
          db.directReadWrite { implicit ctx =>
            cache.set(Height(11), cacheKey, RemoteData.Absence)
            cache.removeFrom(Height(1), cacheKey)
            cache.get(Height(11), cacheKey) shouldBe RemoteData.Unknown
          }
        }
      }

      "returns the last known value before deleted heights" - {
        "cached" in test { (db, cache) =>
          db.directReadWrite { implicit ctx =>
            cache.set(Height(9), cacheKey, cacheValue)
            cache.set(Height(11), cacheKey, RemoteData.Absence)
            cache.removeFrom(Height(10), cacheKey)
            cache.get(Height(10), cacheKey) shouldBe cacheValue
          }
        }

        "absence" in test { (db, cache) =>
          db.directReadWrite { implicit ctx =>
            cache.set(Height(9), cacheKey, RemoteData.Absence)
            cache.set(Height(11), cacheKey, cacheValue)
            cache.removeFrom(Height(10), cacheKey)
            cache.get(Height(10), cacheKey) shouldBe defaultCacheValue
          }
        }

        "unknown if empty" in test { (db, cache) =>
          db.directReadWrite { implicit ctx =>
            cache.set(Height(10), cacheKey, RemoteData.Absence)
            cache.set(Height(11), cacheKey, cacheValue)
            cache.removeFrom(Height(10), cacheKey)
            cache.get(Height(10), cacheKey) shouldBe RemoteData.Unknown
          }
        }
      }
    }
  }

  private def test(f: (RideDbAccess, DiskCache[Address, LeaseBalance]) => Unit): Unit = withDb { db =>
    val caches = db.batchedReadWrite(DefaultDiskCaches(db)(_))
    f(db, caches.accountLeaseBalances)
  }
}
