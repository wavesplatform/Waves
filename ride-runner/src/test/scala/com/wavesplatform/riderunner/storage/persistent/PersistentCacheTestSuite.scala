package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.meta.getSimpleName
import com.wavesplatform.riderunner.storage.Storage

abstract class PersistentCacheTestSuite[KeyT, ValueT] extends PersistentTestSuite {
  private val name                    = getSimpleName(this).replace("TestSuite", "")
  private lazy val defaultCachedValue = RemoteData.Cached(defaultValue)

  s"$name" - {
    "set and get" - {
      "cached" - {
        "on the first height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(8, defaultKey, defaultCachedValue)
          }

          db.readWrite { implicit ctx =>
            cache.set(11, defaultKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.get(8, defaultKey) shouldBe defaultCachedValue
          }
        }

        "before the max height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(8, defaultKey, defaultCachedValue)
          }

          db.readWrite { implicit ctx =>
            cache.set(11, defaultKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, defaultKey) shouldBe defaultCachedValue
          }
        }

        "on the max height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(9, defaultKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.set(10, defaultKey, defaultCachedValue)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, defaultKey) shouldBe defaultCachedValue
          }
        }

        "after the max height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(10, defaultKey, defaultCachedValue)
          }

          db.readWrite { implicit ctx =>
            cache.get(11, defaultKey) shouldBe defaultCachedValue
          }
        }
      }

      "absence" - {
        "before the max height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(8, defaultKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.set(11, defaultKey, defaultCachedValue)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, defaultKey) shouldBe RemoteData.Absence
          }
        }

        "on the max height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(9, defaultKey, defaultCachedValue)
          }

          db.readWrite { implicit ctx =>
            cache.set(10, defaultKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, defaultKey) shouldBe RemoteData.Absence
          }
        }

        "after the max height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(9, defaultKey, defaultCachedValue)
          }

          db.readWrite { implicit ctx =>
            cache.set(10, defaultKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.get(11, defaultKey) shouldBe RemoteData.Absence
          }
        }
      }

      "unknown" - {
        "on empty" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.get(10, defaultKey) shouldBe RemoteData.Unknown
          }
        }

        "before the first known height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(11, defaultKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, defaultKey) shouldBe RemoteData.Unknown
          }
        }
      }
    }

    "remove" - {
      "the data is not available for 'get' after deletion" - {
        "on removed height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(9, defaultKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.remove(9, defaultKey)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, defaultKey) shouldBe RemoteData.Unknown
          }
        }

        "on next height" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(11, defaultKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.remove(1, defaultKey)
          }

          db.readWrite { implicit ctx =>
            cache.get(11, defaultKey) shouldBe RemoteData.Unknown
          }
        }
      }

      "returns the last known value before deleted heights" - {
        "cached" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(9, defaultKey, defaultCachedValue)
          }

          db.readWrite { implicit ctx =>
            cache.set(11, defaultKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.remove(10, defaultKey)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, defaultKey) shouldBe defaultCachedValue
          }
        }

        "absence" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(9, defaultKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.set(11, defaultKey, defaultCachedValue)
          }

          db.readWrite { implicit ctx =>
            cache.remove(10, defaultKey)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, defaultKey) shouldBe RemoteData.Absence
          }
        }

        "unknown if empty" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(10, defaultKey, RemoteData.Absence)
          }

          db.readWrite { implicit ctx =>
            cache.set(11, defaultKey, defaultCachedValue)
          }

          db.readWrite { implicit ctx =>
            cache.remove(10, defaultKey)
          }

          db.readWrite { implicit ctx =>
            cache.get(10, defaultKey) shouldBe RemoteData.Unknown
          }
        }
      }
    }
  }

  protected def defaultKey: KeyT
  protected def defaultValue: ValueT
  protected def test(f: (Storage, PersistentCache[KeyT, ValueT]) => Unit): Unit
}
