package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.db.{Heights, ReadOnly, ReadWrite}
import com.wavesplatform.state.Height

abstract class PersistentCacheWithHistoryTestSuite[KeyT, ValueT] extends PersistentCacheTestSuite[KeyT, ValueT] {
  s"$testedClassName" - {
    "history" - {
      "empty" in test { (db, _) =>
        db.batchedReadOnly { implicit ctx =>
          getHistory shouldBe empty
        }
      }

      "after set" in test { (db, cache) =>
        db.batchedReadWrite { implicit ctx =>
          cache.set(Height(9), defaultKey, RemoteData.Absence)
        }

        db.batchedReadOnly { implicit ctx =>
          getHistory shouldBe Vector(9)
        }
      }

      def removeTests(removeF: (ReadWrite, PersistentCache[KeyT, ValueT], Height) => Unit): Unit = {
        "lesser height" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(9), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            removeF(ctx, cache, Height(8))
          }

          db.batchedReadOnly { implicit ctx =>
            getHistory shouldBe empty
          }
        }

        "same height" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(9), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            removeF(ctx, cache, Height(9))
          }

          db.batchedReadOnly { implicit ctx =>
            getHistory shouldBe empty
          }
        }

        "greater height" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(9), defaultKey, RemoteData.Absence)
          }

          db.batchedReadWrite { implicit ctx =>
            removeF(ctx, cache, Height(10))
          }

          db.batchedReadOnly { implicit ctx =>
            getHistory shouldBe Vector(9)
          }
        }
      }

      "after removeFrom" - removeTests { (ctx, cache, h) =>
        cache.removeFrom(h, defaultKey)(ctx)
      }

      "after removeAllFrom" - removeTests { (ctx, cache, h) =>
        cache.removeAllFrom(h)(ctx)
      }

      "keeps a number of records limited by a maximum possible rollback" in test { (db, cache) =>
        (2 to 104 by 2).foreach { h =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(h), defaultKey, RemoteData.Cached(defaultValue))
          }
        }

        db.batchedReadWrite { implicit ctx =>
          (2 to 3).foreach { h =>
            cache.get(Height(h), defaultKey) shouldBe RemoteData.Unknown
          }

          cache.get(Height(4), defaultKey) shouldBe RemoteData.Cached(defaultValue)
        }
      }
    }
  }

  protected def getHistory(implicit ctx: ReadOnly): Heights
}
