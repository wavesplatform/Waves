package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.account.{AddressScheme, Alias}
import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.db.ReadWrite
import com.wavesplatform.state.Height

class AliasesDiskCacheTestSuite extends DiskTestSuite {
  private val defaultKey         = mkAliasKey("satoshi")
  private val defaultValue       = aliceAddr
  private val defaultCachedValue = RemoteData.Cached(defaultValue)

  "AliasesDiskCache" - {
    "set and get" - {
      "last set wins" in test { cache => implicit ctx =>
        cache.setAddress(Height(1), defaultKey, defaultCachedValue)
        cache.setAddress(Height(1), defaultKey, RemoteData.Absence)
        cache.getAddress(defaultKey) shouldBe RemoteData.Absence
      }

      "unknown on empty" in test { cache => implicit ctx =>
        cache.getAddress(defaultKey) shouldBe RemoteData.Unknown
      }
    }

    "removeAllFrom" in test { cache => implicit ctx =>
      val k1 = mkAliasKey("samsung")
      val k2 = mkAliasKey("toshiba")

      cache.setAddress(Height(1), defaultKey, RemoteData.Cached(aliceAddr))
      cache.setAddress(Height(3), k1, RemoteData.Cached(bobAddr))
      cache.setAddress(Height(3), k2, RemoteData.Cached(carlAddr))
      cache.removeAllFrom(Height(2)) should contain theSameElementsAs List(k1, k2)
      cache.getAddress(defaultKey) shouldBe RemoteData.Cached(aliceAddr)
      cache.getAddress(k1) shouldBe RemoteData.Unknown
      cache.getAddress(k2) shouldBe RemoteData.Unknown
    }
  }

  private def test(f: AliasDiskCache => ReadWrite => Unit): Unit = withDb { db =>
    db.directReadWrite { implicit ctx =>
      f(DefaultDiskCaches(db).aliases)(ctx)
    }
  }

  private def mkAliasKey(s: String) = Alias(AddressScheme.current.chainId, s)
}
