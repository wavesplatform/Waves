package com.wavesplatform.ride.runner.caches.persistent

import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.db.RideDbAccess
import com.wavesplatform.state.Height

abstract class PersistentCacheTestSuite[KeyT, ValueT] extends PersistentTestSuite {
  protected val testedClassName       = suiteName.replace("TestSuite", "")
  private lazy val defaultCachedValue = RemoteData.Cached(defaultValue)

  s"$testedClassName" - {
    "set and get" - {
      "cached" - {
        "on the first height" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(8), defaultKey, defaultCachedValue)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(11), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.get(Height(8), defaultKey) shouldBe defaultCachedValue
          }
        }

        "before the max height" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(8), defaultKey, defaultCachedValue)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(11), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.get(Height(10), defaultKey) shouldBe defaultCachedValue
          }
        }

        "on the max height" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(9), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(10), defaultKey, defaultCachedValue)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.get(Height(10), defaultKey) shouldBe defaultCachedValue
          }
        }

        "after the max height" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(10), defaultKey, defaultCachedValue)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.get(Height(11), defaultKey) shouldBe defaultCachedValue
          }
        }
      }

      "absence" - {
        "before the max height" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(8), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(11), defaultKey, defaultCachedValue)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.get(Height(10), defaultKey) shouldBe RemoteData.Absence
          }
        }

        "on the max height" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(9), defaultKey, defaultCachedValue)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(10), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.get(Height(10), defaultKey) shouldBe RemoteData.Absence
          }
        }

        "after the max height" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(9), defaultKey, defaultCachedValue)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(10), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.get(Height(11), defaultKey) shouldBe RemoteData.Absence
          }
        }
      }

      "unknown" - {
        "on empty" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.get(Height(10), defaultKey) shouldBe RemoteData.Unknown
          }
        }

        "before the first known height" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(11), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.get(Height(10), defaultKey) shouldBe RemoteData.Unknown
          }
        }
      }
    }

    "remove" - {
      "the data is not available for 'get' after deletion" - {
        "on removed height" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(9), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.removeFrom(Height(9), defaultKey)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.get(Height(10), defaultKey) shouldBe RemoteData.Unknown
          }
        }

        "on next height" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(11), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.removeFrom(Height(1), defaultKey)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.get(Height(11), defaultKey) shouldBe RemoteData.Unknown
          }
        }
      }

      "returns the last known value before deleted heights" - {
        "cached" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(9), defaultKey, defaultCachedValue)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(11), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.removeFrom(Height(10), defaultKey)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.get(Height(10), defaultKey) shouldBe defaultCachedValue
          }
        }

        "absence" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(9), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(11), defaultKey, defaultCachedValue)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.removeFrom(Height(10), defaultKey)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.get(Height(10), defaultKey) shouldBe RemoteData.Absence
          }
        }

        "unknown if empty" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(10), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(11), defaultKey, defaultCachedValue)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.removeFrom(Height(10), defaultKey)
          }

          db.batchedReadWrite { implicit ctx =>
            cache.get(Height(10), defaultKey) shouldBe RemoteData.Unknown
          }
        }
      }
    }
  }

  protected def defaultKey: KeyT
  protected def defaultValue: ValueT
  protected def test(f: (RideDbAccess, PersistentCache[KeyT, ValueT]) => Unit): Unit
}
