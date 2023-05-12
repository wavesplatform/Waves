package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.ride.runner.db.RideDbAccess
import com.wavesplatform.ride.runner.storage.RemoteData
import com.wavesplatform.state.{Height, TransactionId}

class TransactionsPersistentCacheTestSuite extends PersistentTestSuite {
  private val defaultTxId        = mkTxId(0)
  private val defaultHeight      = Height(10)
  private val defaultCachedValue = RemoteData.Cached(defaultHeight)

  "TransactionsPersistentCache" - {
    "set and get" - {
      "last set wins" in test { (db, cache) =>
        db.batchedReadWrite { implicit ctx =>
          cache.setHeight(defaultTxId, defaultCachedValue)
        }

        db.batchedReadWrite { implicit ctx =>
          cache.setHeight(defaultTxId, RemoteData.Absence)
        }

        db.batchedReadOnly { implicit ctx =>
          cache.getHeight(defaultTxId) shouldBe RemoteData.Absence
        }
      }

      "unknown on empty" in test { (db, cache) =>
        db.batchedReadOnly { implicit ctx =>
          cache.getHeight(defaultTxId) shouldBe RemoteData.Unknown
        }
      }
    }

    "getAllKeys" in test { (db, cache) =>
      val tx1 = mkTxId(1)
      val tx2 = mkTxId(2)

      db.batchedReadWrite { implicit ctx =>
        cache.setHeight(defaultTxId, RemoteData.Cached(Height(1)))
        cache.setHeight(tx1, RemoteData.Cached(Height(3)))
      // TODO #121 move tx2 here
      }

      db.batchedReadWrite { implicit ctx =>
        cache.setHeight(tx2, RemoteData.Cached(Height(3)))
      }

      db.batchedReadOnly { implicit ctx =>
        withClue("from 1") { cache.getAllKeys(Height(1)) should contain theSameElementsAs List(defaultTxId, tx1, tx2) }
        withClue("from 2") { cache.getAllKeys(Height(2)) should contain theSameElementsAs List(tx1, tx2) }
      }
    }

    "removeAllFrom" in test { (db, cache) =>
      val tx1 = mkTxId(1)
      val tx2 = mkTxId(2)

      db.batchedReadWrite { implicit ctx =>
        cache.setHeight(defaultTxId, RemoteData.Cached(Height(1)))
        cache.setHeight(tx1, RemoteData.Cached(Height(3)))
      // TODO #121 move tx2 here
      }

      db.batchedReadWrite { implicit ctx =>
        cache.setHeight(tx2, RemoteData.Cached(Height(3)))
      }

      db.batchedReadWrite { implicit ctx =>
        cache.removeAllFrom(Height(2)) should contain theSameElementsAs List(tx1, tx2)
      }

      db.batchedReadOnly { implicit ctx =>
        cache.getHeight(defaultTxId) shouldBe RemoteData.Cached(Height(1))
        cache.getHeight(tx1) shouldBe RemoteData.Unknown
        cache.getHeight(tx2) shouldBe RemoteData.Unknown

        cache.getAllKeys(Height(1)) should contain theSameElementsAs List(defaultTxId)
      }
    }
  }

  private def mkTxId(n: Byte) = TransactionId(ByteStr(Array.fill[Byte](DigestLength)(n)))

  private def test(f: (RideDbAccess, TransactionPersistentCache) => Unit): Unit = withDb { db =>
    val caches = db.batchedReadOnly(DefaultPersistentCaches(db)(_))
    f(db, caches.transactions)
  }
}
