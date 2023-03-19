package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.account.{Address, AddressScheme, Alias}

class AliasesPersistentCacheTestSuite extends PersistentCacheTestSuite[Alias, Address] {
  protected override val defaultKey = Alias(AddressScheme.current.chainId, "satoshi")

  protected override val defaultValue = alice.toAddress

  protected override def test(f: (PersistentStorage, PersistentCache[Alias, Address]) => Unit): Unit = withDb { db =>
    val caches = db.readOnly(DefaultPersistentCaches(db)(_))
    f(db, caches.aliases)
  }
}
