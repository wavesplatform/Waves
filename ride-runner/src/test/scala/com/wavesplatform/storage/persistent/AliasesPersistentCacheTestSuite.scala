package com.wavesplatform.storage.persistent

import com.wavesplatform.account.{Address, AddressScheme, Alias}

class AliasesPersistentCacheTestSuite extends PersistentCacheTestSuite[Alias, Address] {
  protected override val defaultKey = Alias(AddressScheme.current.chainId, "satoshi")

  protected override val defaultValue = alice.toAddress

  protected override def test(f: PersistentCache[Alias, Address] => Unit): Unit = withDb { db =>
    val caches = new LevelDbPersistentCaches(db)
    f(caches.aliases)
  }
}
