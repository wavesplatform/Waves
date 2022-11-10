package com.wavesplatform.storage.persistent

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.meta.getSimpleName

abstract class PersistentCacheTestSuite[KeyT, ValueT] extends BaseTestSuite with HasLevelDb {
  private val name                    = getSimpleName(this).replace("TestSuite", "")
  private lazy val defaultCachedValue = RemoteData.Cached(defaultValue)

  s"$name" - {
    "set and get" - {
      "cached" - {
        "on the first height" in test { cache =>
          cache.set(8, defaultPair, defaultCachedValue)
          cache.set(11, defaultPair, RemoteData.Absence)

          cache.get(8, defaultPair) shouldBe defaultCachedValue
        }

        "before the max height" in test { cache =>
          cache.set(8, defaultPair, defaultCachedValue)
          cache.set(11, defaultPair, RemoteData.Absence)

          cache.get(10, defaultPair) shouldBe defaultCachedValue
        }

        "on the max height" in test { cache =>
          cache.set(9, defaultPair, RemoteData.Absence)
          cache.set(10, defaultPair, defaultCachedValue)

          cache.get(10, defaultPair) shouldBe defaultCachedValue
        }

        "after the max height" in test { cache =>
          cache.set(10, defaultPair, defaultCachedValue)
          cache.get(11, defaultPair) shouldBe defaultCachedValue
        }
      }

      "absence" - {
        "before the max height" in test { cache =>
          cache.set(8, defaultPair, RemoteData.Absence)
          cache.set(11, defaultPair, defaultCachedValue)

          cache.get(10, defaultPair) shouldBe RemoteData.Absence
        }

        "on the max height" in test { cache =>
          cache.set(9, defaultPair, defaultCachedValue)
          cache.set(10, defaultPair, RemoteData.Absence)

          cache.get(10, defaultPair) shouldBe RemoteData.Absence
        }

        "after the max height" in test { cache =>
          cache.set(9, defaultPair, defaultCachedValue)
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
          cache.set(9, defaultPair, defaultCachedValue)
          cache.set(11, defaultPair, RemoteData.Absence)
          cache.remove(10, defaultPair)

          cache.get(10, defaultPair) shouldBe defaultCachedValue
        }

        "absence" in test { cache =>
          cache.set(9, defaultPair, RemoteData.Absence)
          cache.set(11, defaultPair, defaultCachedValue)
          cache.remove(10, defaultPair)

          cache.get(10, defaultPair) shouldBe RemoteData.Absence
        }

        "unknown if empty" in test { cache =>
          cache.set(10, defaultPair, RemoteData.Absence)
          cache.set(11, defaultPair, defaultCachedValue)
          cache.remove(10, defaultPair)

          cache.get(10, defaultPair) shouldBe RemoteData.Unknown
        }
      }
    }
  }

  protected def defaultPair: KeyT
  protected def defaultValue: ValueT
  protected def test(f: PersistentCache[KeyT, ValueT] => Unit): Unit
}
