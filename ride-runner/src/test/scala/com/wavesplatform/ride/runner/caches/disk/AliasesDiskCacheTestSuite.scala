package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.account.{AddressScheme, Alias}
import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.db.RideDbAccess
import com.wavesplatform.state.Height

class AliasesDiskCacheTestSuite extends DiskTestSuite {
  private val defaultKey         = mkAliasKey("satoshi")
  private val defaultValue       = aliceAddr
  private val defaultCachedValue = RemoteData.Cached(defaultValue)

  "AliasesDiskCache" - {
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

    "removeAllFrom" in test { (db, cache) =>
      val k1 = mkAliasKey("samsung")
      val k2 = mkAliasKey("toshiba")

      db.batchedReadWrite { implicit ctx =>
        cache.setAddress(Height(1), defaultKey, RemoteData.Cached(aliceAddr))
        cache.setAddress(Height(3), k1, RemoteData.Cached(bobAddr))
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
      }
    }
  }

  private def test(f: (RideDbAccess, AliasDiskCache) => Unit): Unit = withDb { db =>
    val caches = db.batchedReadOnly(DefaultDiskCaches(db)(_))
    f(db, caches.aliases)
  }

  private def mkAliasKey(s: String) = Alias(AddressScheme.current.chainId, s)
}
