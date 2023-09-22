package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.db.RideDbAccess
import com.wavesplatform.state.{Height, TransactionId}

class TransactionsDiskCacheTestSuite extends DiskTestSuite {
  private val defaultKey         = mkTxKey(0)
  private val defaultHeight      = Height(10)
  private val defaultCachedValue = RemoteData.Cached(defaultHeight)

  "TransactionsDiskCache" - {
    "set and get" - {
      "last set wins" in test { (db, cache) =>
        db.directReadWrite { implicit ctx =>
          cache.setHeight(defaultKey, defaultCachedValue)
          cache.setHeight(defaultKey, RemoteData.Absence)
          cache.getHeight(defaultKey) shouldBe RemoteData.Absence
        }
      }

      "unknown on empty" in test { (db, cache) =>
        db.directReadOnly { implicit ctx =>
          cache.getHeight(defaultKey) shouldBe RemoteData.Unknown
        }
      }
    }

    "removeAllFrom" in test { (db, cache) =>
      val k1 = mkTxKey(1)
      val k2 = mkTxKey(2)

      db.directReadWrite { implicit ctx =>
        cache.setHeight(defaultKey, RemoteData.Cached(Height(1)))
        cache.setHeight(k1, RemoteData.Cached(Height(3)))
        cache.setHeight(k2, RemoteData.Cached(Height(3)))
        cache.removeAllFrom(Height(2)) should contain theSameElementsAs List(k1, k2)
        cache.getHeight(defaultKey) shouldBe RemoteData.Cached(Height(1))
        cache.getHeight(k1) shouldBe RemoteData.Unknown
        cache.getHeight(k2) shouldBe RemoteData.Unknown
      }
    }
  }

  private def mkTxKey(n: Byte) = TransactionId(ByteStr(Array.fill[Byte](DigestLength)(n)))

  private def test(f: (RideDbAccess, TransactionDiskCache) => Unit): Unit = withDb { db =>
    val caches = db.batchedReadOnly(DefaultDiskCaches(db)(_))
    f(db, caches.transactions)
  }
}
