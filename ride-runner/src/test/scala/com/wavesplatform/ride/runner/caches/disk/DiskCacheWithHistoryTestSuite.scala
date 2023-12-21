package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.db.{Heights, ReadOnly, ReadWrite}
import com.wavesplatform.state.Height

abstract class DiskCacheWithHistoryTestSuite[KeyT, ValueT] extends DiskCacheTestSuite[KeyT, ValueT] {
  testedClassName - {
    "history" - {
      "empty" in test { _ => implicit ctx =>
        getHistory shouldBe empty
      }

      "after set" in test { cache => implicit ctx =>
        cache.set(Height(9), defaultKey, RemoteData.Absence)
        getHistory shouldBe Vector(9)
      }

      def removeTests(removeF: (ReadWrite, DiskCache[KeyT, ValueT], Height) => Unit): Unit = {
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
          cache.set(Height(h), defaultKey, RemoteData.Cached(defaultValue))
        }

        (2 to 3).foreach { h =>
          cache.get(Height(h), defaultKey) shouldBe RemoteData.Unknown
        }

        cache.get(Height(4), defaultKey) shouldBe RemoteData.Cached(defaultValue)
      }
    }
  }

  protected def getHistory(implicit ctx: ReadOnly): Heights
}
