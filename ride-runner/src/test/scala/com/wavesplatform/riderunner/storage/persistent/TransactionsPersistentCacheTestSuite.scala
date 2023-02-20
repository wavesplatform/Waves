package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.state.{Height, TransactionId}

class TransactionsPersistentCacheTestSuite extends PersistentTestSuite {
  private val defaultTxId        = TransactionId @@ ByteStr(Array.fill[Byte](DigestLength)(0))
  private val defaultHeight      = Height @@ 10
  private val defaultCachedValue = RemoteData.Cached(defaultHeight)

  "TransactionsPersistentCache" - {
    "set and get" - {
      "last set wins" in test { cache =>
        cache.setHeight(defaultTxId, defaultCachedValue)
        cache.setHeight(defaultTxId, RemoteData.Absence)

        cache.getHeight(defaultTxId) shouldBe RemoteData.Absence
      }

      "unknown on empty" in test { cache =>
        cache.getHeight(defaultTxId) shouldBe RemoteData.Unknown
      }
    }

    "remove" - {
      "the data is not available for 'get' after deletion" - test { cache =>
        cache.setHeight(defaultTxId, defaultCachedValue)
        cache.remove(defaultTxId)

        cache.getHeight(defaultTxId) shouldBe RemoteData.Unknown
      }
    }
  }

  private def test(f: TransactionPersistentCache => Unit): Unit = withDb { db =>
    val caches = new LevelDbPersistentCaches(db)
    f(caches.transactions)
  }
}
