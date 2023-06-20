package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.account.{AddressScheme, Alias}
import com.wavesplatform.ride.runner.db.RideDbAccess
import com.wavesplatform.ride.runner.storage.{CacheKey, RemoteData}
import com.wavesplatform.state.Height

class AliasesPersistentCacheTestSuite extends PersistentTestSuite {
  private val defaultKey         = mkAliasKey("satoshi")
  private val defaultValue       = alice.toAddress
  private val defaultCachedValue = RemoteData.Cached(defaultValue)

  "AliasesPersistentCache" - {
    "set and get" - {
      "last set wins" in test { (db, cache) =>
        db.batchedReadWrite { implicit ctx =>
          cache.setAddress(Height(1), defaultKey, defaultCachedValue)
        }

        db.batchedReadWrite { implicit ctx =>
          cache.setAddress(Height(1), defaultKey, RemoteData.Absence)
        }

        db.batchedReadOnly { implicit ctx =>
          cache.getAddress(defaultKey) shouldBe RemoteData.Absence
        }
      }

      "unknown on empty" in test { (db, cache) =>
        db.batchedReadOnly { implicit ctx =>
          cache.getAddress(defaultKey) shouldBe RemoteData.Unknown
        }
      }
    }

    "getAllKeys" in test { (db, cache) =>
      val k1 = mkAliasKey("samsung")
      val k2 = mkAliasKey("toshiba")

      db.batchedReadWrite { implicit ctx =>
        cache.setAddress(Height(1), defaultKey, RemoteData.Cached(aliceAddr))
        cache.setAddress(Height(3), k1, RemoteData.Cached(bobAddr))
      // TODO #121 move k2 here
      }

      db.batchedReadWrite { implicit ctx =>
        cache.setAddress(Height(3), k2, RemoteData.Cached(carlAddr))
      }

      db.batchedReadOnly { implicit ctx =>
        withClue("from 1") { cache.getAllKeys(Height(1)) should contain theSameElementsAs List(defaultKey, k1, k2) }
        withClue("from 2") { cache.getAllKeys(Height(2)) should contain theSameElementsAs List(k1, k2) }
      }
    }

    "removeAllFrom" in test { (db, cache) =>
      val k1 = mkAliasKey("samsung")
      val k2 = mkAliasKey("toshiba")

      db.batchedReadWrite { implicit ctx =>
        cache.setAddress(Height(1), defaultKey, RemoteData.Cached(aliceAddr))
        cache.setAddress(Height(3), k1, RemoteData.Cached(bobAddr))
      // TODO #121 move k2 here
      }

      db.batchedReadWrite { implicit ctx =>
        cache.setAddress(Height(3), k2, RemoteData.Cached(carlAddr))
      }

      db.batchedReadWrite { implicit ctx =>
        cache.removeAllFrom(Height(2)) should contain theSameElementsAs List(k1, k2)
      }

      db.batchedReadOnly { implicit ctx =>
        cache.getAddress(defaultKey) shouldBe RemoteData.Cached(aliceAddr)
        cache.getAddress(k1) shouldBe RemoteData.Unknown
        cache.getAddress(k2) shouldBe RemoteData.Unknown

        cache.getAllKeys(Height(1)) should contain theSameElementsAs List(defaultKey)
      }
    }
  }

  private def test(f: (RideDbAccess, AliasPersistentCache) => Unit): Unit = withDb { db =>
    val caches = db.batchedReadOnly(DefaultPersistentCaches(db)(_))
    f(db, caches.aliases)
  }

  private def mkAliasKey(s: String) = CacheKey.Alias(Alias(AddressScheme.current.chainId, s))
}