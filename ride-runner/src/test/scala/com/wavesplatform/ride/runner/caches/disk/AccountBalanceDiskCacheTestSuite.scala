package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.AddressId
import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.db.{Heights, ReadOnly, ReadWrite}
import com.wavesplatform.state.Height
import com.wavesplatform.transaction.{Asset, AssetIdLength}

class AccountBalanceDiskCacheTestSuite extends DiskTestSuite {
  private val defaultCachedValue: RemoteData[Long] = RemoteData.Cached(1L)
  private val defaultCacheValue: RemoteData[Long]  = RemoteData.Cached(0L) // Equal to RemoteData.Absence for this case

  "AccountBalanceDiskCache" - {
    "with Waves" - tests(Asset.Waves)
    "with IssuedAsset" - tests(Asset.IssuedAsset(ByteStr(Array.fill[Byte](AssetIdLength)(0))))
  }

  private def tests(asset: Asset): Unit = {
    val defaultKey                                  = (aliceAddr, asset)
    def getHistory(implicit ctx: ReadOnly): Heights = ctx.getOpt(KvPairs.AccountAssetsHistory.at((AddressId(0L), asset))).getOrElse(Vector.empty)

    "history" - {
      "empty" in test { _ => implicit ctx =>
        getHistory shouldBe empty
      }

      "after set" in test { cache => implicit ctx =>
        cache.set(Height(9), defaultKey, RemoteData.Absence)
        getHistory shouldBe Vector(9)
      }

      def removeTests(removeF: (ReadWrite, DiskCache[(Address, Asset), Long], Height) => Unit): Unit = {
        "lesser height" in test { cache => implicit ctx =>
          cache.set(Height(9), defaultKey, RemoteData.Absence)
          removeF(ctx, cache, Height(8))
          getHistory shouldBe empty
        }

        "same height" in test { cache => implicit ctx =>
          cache.set(Height(9), defaultKey, RemoteData.Absence)
          removeF(ctx, cache, Height(9))
          getHistory shouldBe empty
        }

        "greater height" in test { cache => implicit ctx =>
          cache.set(Height(9), defaultKey, RemoteData.Absence)
          removeF(ctx, cache, Height(10))
          getHistory shouldBe Vector(9)
        }
      }

      "after removeFrom" - removeTests { (ctx, cache, h) =>
        cache.removeFrom(h, defaultKey)(ctx)
      }

      "after removeAllFrom" - removeTests { (ctx, cache, h) =>
        cache.removeAllFrom(h)(ctx)
      }

      "keeps a number of records limited by a maximum possible rollback" in test { cache => implicit ctx =>
        (2 to 104 by 2).foreach { h =>
          cache.set(Height(h), defaultKey, RemoteData.Cached(h))
        }

        (2 to 3).foreach { h =>
          cache.get(Height(h), defaultKey) shouldBe RemoteData.Unknown
        }

        cache.get(Height(4), defaultKey) shouldBe RemoteData.Cached(4)
      }
    }

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
          cache.get(Height(10), defaultKey) shouldBe defaultCacheValue
        }

        "on the max height" in test { cache => implicit ctx =>
          cache.set(Height(9), defaultKey, defaultCachedValue)
          cache.set(Height(10), defaultKey, RemoteData.Absence)
          cache.get(Height(10), defaultKey) shouldBe defaultCacheValue
        }

        "after the max height" in test { cache => implicit ctx =>
          cache.set(Height(9), defaultKey, defaultCachedValue)
          cache.set(Height(10), defaultKey, RemoteData.Absence)
          cache.get(Height(11), defaultKey) shouldBe defaultCacheValue
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
          cache.get(Height(10), defaultKey) shouldBe defaultCacheValue
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

  private def test(f: DiskCache[(Address, Asset), Long] => ReadWrite => Unit): Unit = withDb { db =>
    db.directReadWrite { implicit ctx =>
      f(DefaultDiskCaches(db).accountBalances)(ctx)
    }
  }
}
