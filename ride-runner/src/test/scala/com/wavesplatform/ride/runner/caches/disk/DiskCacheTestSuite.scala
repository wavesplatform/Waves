package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.db.ReadWrite
import com.wavesplatform.state.Height

abstract class DiskCacheTestSuite[KeyT, ValueT] extends DiskTestSuite {
  protected val testedClassName       = suiteName.replace("TestSuite", "")
  private lazy val defaultCachedValue = RemoteData.Cached(defaultValue)

  testedClassName - {
    "set and get" - {
      "cached" - {
        "on the first height" in test { cache => implicit ctx =>
          cache.set(Height(8), defaultKey, defaultCachedValue)
          cache.set(Height(11), defaultKey, RemoteData.Absence)
          cache.get(Height(8), defaultKey) shouldBe defaultCachedValue
        }

        "before the max height" in test { cache => implicit ctx =>
          cache.set(Height(8), defaultKey, defaultCachedValue)
          cache.set(Height(11), defaultKey, RemoteData.Absence)
          cache.get(Height(10), defaultKey) shouldBe defaultCachedValue
        }

        "on the max height" in test { cache => implicit ctx =>
          cache.set(Height(9), defaultKey, RemoteData.Absence)
          cache.set(Height(10), defaultKey, defaultCachedValue)
          cache.get(Height(10), defaultKey) shouldBe defaultCachedValue
        }

        "after the max height" in test { cache => implicit ctx =>
          cache.set(Height(10), defaultKey, defaultCachedValue)
          cache.get(Height(11), defaultKey) shouldBe defaultCachedValue
        }
      }

      "absence" - {
        "before the max height" in test { cache => implicit ctx =>
          cache.set(Height(8), defaultKey, RemoteData.Absence)
          cache.set(Height(11), defaultKey, defaultCachedValue)
          cache.get(Height(10), defaultKey) shouldBe RemoteData.Absence
        }

        "on the max height" in test { cache => implicit ctx =>
          cache.set(Height(9), defaultKey, defaultCachedValue)
          cache.set(Height(10), defaultKey, RemoteData.Absence)
          cache.get(Height(10), defaultKey) shouldBe RemoteData.Absence
        }

        "after the max height" in test { cache => implicit ctx =>
          cache.set(Height(9), defaultKey, defaultCachedValue)
          cache.set(Height(10), defaultKey, RemoteData.Absence)
          cache.get(Height(11), defaultKey) shouldBe RemoteData.Absence
        }
      }

      "unknown" - {
        "on empty" in test { cache => implicit ctx =>
          cache.get(Height(10), defaultKey) shouldBe RemoteData.Unknown
        }

        "before the first known height" in test { cache => implicit ctx =>
          cache.set(Height(11), defaultKey, RemoteData.Absence)
          cache.get(Height(10), defaultKey) shouldBe RemoteData.Unknown
        }
      }
    }

    "remove" - {
      "the data is not available for 'get' after deletion" - {
        "on removed height" in test { cache => implicit ctx =>
          cache.set(Height(9), defaultKey, RemoteData.Absence)
          cache.removeFrom(Height(9), defaultKey)
          cache.get(Height(10), defaultKey) shouldBe RemoteData.Unknown
        }

        "on next height" in test { cache => implicit ctx =>
          cache.set(Height(11), defaultKey, RemoteData.Absence)
          cache.removeFrom(Height(1), defaultKey)
          cache.get(Height(11), defaultKey) shouldBe RemoteData.Unknown
        }
      }

      "returns the last known value before deleted heights" - {
        "cached" in test { cache => implicit ctx =>
          cache.set(Height(9), defaultKey, defaultCachedValue)
          cache.set(Height(11), defaultKey, RemoteData.Absence)
          cache.removeFrom(Height(10), defaultKey)
          cache.get(Height(10), defaultKey) shouldBe defaultCachedValue
        }

        "absence" in test { cache => implicit ctx =>
          cache.set(Height(9), defaultKey, RemoteData.Absence)
          cache.set(Height(11), defaultKey, defaultCachedValue)
          cache.removeFrom(Height(10), defaultKey)
          cache.get(Height(10), defaultKey) shouldBe RemoteData.Absence
        }

        "unknown if empty" in test { cache => implicit ctx =>
          cache.set(Height(10), defaultKey, RemoteData.Absence)
          cache.set(Height(11), defaultKey, defaultCachedValue)
          cache.removeFrom(Height(10), defaultKey)
          cache.get(Height(10), defaultKey) shouldBe RemoteData.Unknown
        }
      }
    }
  }

  protected def defaultKey: KeyT
  protected def defaultValue: ValueT
  protected def test(f: DiskCache[KeyT, ValueT] => ReadWrite => Unit): Unit
}
