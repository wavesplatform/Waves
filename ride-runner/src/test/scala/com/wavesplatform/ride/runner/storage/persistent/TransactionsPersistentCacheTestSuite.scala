package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.ride.runner.db.RideDbAccess
import com.wavesplatform.ride.runner.storage.RemoteData
import com.wavesplatform.state.{Height, TransactionId}

class TransactionsPersistentCacheTestSuite extends PersistentTestSuite {
  private val defaultTxId        = TransactionId @@ ByteStr(Array.fill[Byte](DigestLength)(0))
  private val defaultHeight      = Height @@ 10
  private val defaultCachedValue = RemoteData.Cached(defaultHeight)

  "TransactionsPersistentCache" - {
    "set and get" - {
      "last set wins" in test { (db, cache) =>
        db.readWrite { implicit ctx =>
          cache.setHeight(defaultTxId, defaultCachedValue)
        }

        db.readWrite { implicit ctx =>
          cache.setHeight(defaultTxId, RemoteData.Absence)
        }

        db.readOnly { implicit ctx =>
          cache.getHeight(defaultTxId) shouldBe RemoteData.Absence
        }
      }

      "unknown on empty" in test { (db, cache) =>
        db.readOnly { implicit ctx =>
          cache.getHeight(defaultTxId) shouldBe RemoteData.Unknown
        }
      }
    }

    "remove" - {
      "the data is not available for 'get' after deletion" - test { (db, cache) =>
        db.readWrite { implicit ctx =>
          cache.setHeight(defaultTxId, defaultCachedValue)
        }

        db.readWrite { implicit ctx =>
          cache.remove(defaultTxId)
        }

        db.readOnly { implicit ctx =>
          cache.getHeight(defaultTxId) shouldBe RemoteData.Unknown
        }
      }
    }
  }

  private def test(f: (RideDbAccess, TransactionPersistentCache) => Unit): Unit = withDb { db =>
    val caches = db.readOnly(DefaultPersistentCaches(db)(_))
    f(db, caches.transactions)
  }
}
