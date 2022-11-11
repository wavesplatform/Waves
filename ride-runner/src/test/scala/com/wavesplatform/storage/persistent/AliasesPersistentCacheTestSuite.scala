package com.wavesplatform.storage.persistent

import com.wavesplatform.account.{Address, AddressScheme, Alias}
import com.wavesplatform.wallet.Wallet

import java.nio.charset.StandardCharsets

class AliasesPersistentCacheTestSuite extends PersistentCacheTestSuite[Alias, Address] {
  private val alice = Wallet.generateNewAccount("test".getBytes(StandardCharsets.UTF_8), 0)

  protected override val defaultKey   = Alias(AddressScheme.current.chainId, "satoshi")
  protected override val defaultValue = alice.toAddress

  protected override def test(f: PersistentCache[Alias, Address] => Unit): Unit = withDb { db =>
    val caches = new LevelDbPersistentCaches(db)
    f(caches.aliases)
  }
}
