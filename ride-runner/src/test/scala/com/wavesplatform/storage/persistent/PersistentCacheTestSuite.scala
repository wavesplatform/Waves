package com.wavesplatform.storage.persistent

import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.meta.getSimpleName

abstract class PersistentCacheTestSuite[KeyT, ValueT] extends PersistentTestSuite {
  private val name                    = getSimpleName(this).replace("TestSuite", "")
  private lazy val defaultCachedValue = RemoteData.Cached(defaultValue)

  s"$name" - {
    "set and get" - {
      "cached" - {
        "on the first height" in test { cache =>
          cache.set(8, defaultKey, defaultCachedValue)
          cache.set(11, defaultKey, RemoteData.Absence)

          cache.get(8, defaultKey) shouldBe defaultCachedValue
        }

        "before the max height" in test { cache =>
          cache.set(8, defaultKey, defaultCachedValue)
          cache.set(11, defaultKey, RemoteData.Absence)

          cache.get(10, defaultKey) shouldBe defaultCachedValue
        }

        "on the max height" in test { cache =>
          cache.set(9, defaultKey, RemoteData.Absence)
          cache.set(10, defaultKey, defaultCachedValue)

          cache.get(10, defaultKey) shouldBe defaultCachedValue
        }

        "after the max height" in test { cache =>
          cache.set(10, defaultKey, defaultCachedValue)
          cache.get(11, defaultKey) shouldBe defaultCachedValue
        }
      }

      "absence" - {
        "before the max height" in test { cache =>
          cache.set(8, defaultKey, RemoteData.Absence)
          cache.set(11, defaultKey, defaultCachedValue)

          cache.get(10, defaultKey) shouldBe RemoteData.Absence
        }

        "on the max height" in test { cache =>
          cache.set(9, defaultKey, defaultCachedValue)
          cache.set(10, defaultKey, RemoteData.Absence)

          cache.get(10, defaultKey) shouldBe RemoteData.Absence
        }

        "after the max height" in test { cache =>
          cache.set(9, defaultKey, defaultCachedValue)
          cache.set(10, defaultKey, RemoteData.Absence)

          cache.get(11, defaultKey) shouldBe RemoteData.Absence
        }
      }

      "unknown" - {
        "on empty" in test { cache =>
          cache.get(10, defaultKey) shouldBe RemoteData.Unknown
        }

        "before the first known height" in test { cache =>
          cache.set(11, defaultKey, RemoteData.Absence)
          cache.get(10, defaultKey) shouldBe RemoteData.Unknown
        }
      }
    }

    "remove" - {
      "the data is not available for 'get' after deletion" - {
        "on removed height" in test { cache =>
          cache.set(9, defaultKey, RemoteData.Absence)
          cache.remove(9, defaultKey)

          cache.get(10, defaultKey) shouldBe RemoteData.Unknown
        }

        "on next height" in test { cache =>
          cache.set(11, defaultKey, RemoteData.Absence)
          cache.remove(1, defaultKey)

          cache.get(11, defaultKey) shouldBe RemoteData.Unknown
        }
      }

      "returns the last known value before deleted heights" - {
        "cached" in test { cache =>
          cache.set(9, defaultKey, defaultCachedValue)
          cache.set(11, defaultKey, RemoteData.Absence)
          cache.remove(10, defaultKey)

          cache.get(10, defaultKey) shouldBe defaultCachedValue
        }

        "absence" in test { cache =>
          cache.set(9, defaultKey, RemoteData.Absence)
          cache.set(11, defaultKey, defaultCachedValue)
          cache.remove(10, defaultKey)

          cache.get(10, defaultKey) shouldBe RemoteData.Absence
        }

        "unknown if empty" in test { cache =>
          cache.set(10, defaultKey, RemoteData.Absence)
          cache.set(11, defaultKey, defaultCachedValue)
          cache.remove(10, defaultKey)

          cache.get(10, defaultKey) shouldBe RemoteData.Unknown
        }
      }
    }
  }

  protected def defaultKey: KeyT
  protected def defaultValue: ValueT
  protected def test(f: PersistentCache[KeyT, ValueT] => Unit): Unit
}
