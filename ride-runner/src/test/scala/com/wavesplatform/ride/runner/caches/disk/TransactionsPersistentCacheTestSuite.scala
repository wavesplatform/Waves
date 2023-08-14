package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.ride.runner.caches.{CacheKey, RemoteData}
import com.wavesplatform.ride.runner.db.RideDbAccess
import com.wavesplatform.state.{Height, TransactionId}

class TransactionsPersistentCacheTestSuite extends PersistentTestSuite {
  private val defaultKey         = mkTxKey(0)
  private val defaultHeight      = Height(10)
  private val defaultCachedValue = RemoteData.Cached(defaultHeight)

  "TransactionsPersistentCache" - {
    "set and get" - {
      "last set wins" in test { (db, cache) =>
        db.batchedReadWrite { implicit ctx =>
          cache.setHeight(defaultKey, defaultCachedValue)
        }

        db.batchedReadWrite { implicit ctx =>
          cache.setHeight(defaultKey, RemoteData.Absence)
        }

        db.batchedReadOnly { implicit ctx =>
          cache.getHeight(defaultKey) shouldBe RemoteData.Absence
        }
      }

      "unknown on empty" in test { (db, cache) =>
        db.batchedReadOnly { implicit ctx =>
          cache.getHeight(defaultKey) shouldBe RemoteData.Unknown
        }
      }
    }

    "removeAllFrom" in test { (db, cache) =>
      val k1 = mkTxKey(1)
      val k2 = mkTxKey(2)

      db.batchedReadWrite { implicit ctx =>
        cache.setHeight(defaultKey, RemoteData.Cached(Height(1)))
        cache.setHeight(k1, RemoteData.Cached(Height(3)))
      }

      db.batchedReadWrite { implicit ctx =>
        cache.setHeight(k2, RemoteData.Cached(Height(3)))
      }

      db.batchedReadWrite { implicit ctx =>
        cache.removeAllFrom(Height(2)) should contain theSameElementsAs List(k1, k2)
      }

      db.batchedReadOnly { implicit ctx =>
        cache.getHeight(defaultKey) shouldBe RemoteData.Cached(Height(1))
        cache.getHeight(k1) shouldBe RemoteData.Unknown
        cache.getHeight(k2) shouldBe RemoteData.Unknown
      }
    }
  }

  private def mkTxKey(n: Byte) = CacheKey.Transaction(TransactionId(ByteStr(Array.fill[Byte](DigestLength)(n))))

  private def test(f: (RideDbAccess, TransactionPersistentCache) => Unit): Unit = withDb { db =>
    val caches = db.batchedReadOnly(DefaultPersistentCaches(db)(_))
    f(db, caches.transactions)
  }
}
