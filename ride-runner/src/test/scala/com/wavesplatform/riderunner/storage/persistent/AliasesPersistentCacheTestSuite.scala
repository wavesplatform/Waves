package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.account.{Address, AddressScheme, Alias}
import com.wavesplatform.riderunner.storage.Storage

class AliasesPersistentCacheTestSuite extends PersistentCacheTestSuite[Alias, Address] {
  protected override val defaultKey = Alias(AddressScheme.current.chainId, "satoshi")

  protected override val defaultValue = alice.toAddress

  protected override def test(f: (Storage, PersistentCache[Alias, Address]) => Unit): Unit = withDb { db =>
    val caches = db.readOnly(LevelDbPersistentCaches(db)(_))
    f(db, caches.aliases)
  }
}
