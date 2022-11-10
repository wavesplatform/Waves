package com.wavesplatform.storage.persistent

import com.wavesplatform.state.{BooleanDataEntry, DataEntry}
import com.wavesplatform.storage.AccountDataKey
import com.wavesplatform.wallet.Wallet

import java.nio.charset.StandardCharsets

class AccountDataPersistentCacheTestSuite extends PersistentCacheTestSuite[AccountDataKey, DataEntry[?]] {
  private val alice = Wallet.generateNewAccount("test".getBytes(StandardCharsets.UTF_8), 0)

  private val defaultPairDataKey      = "foo"
  protected override val defaultPair  = (alice.publicKey.toAddress, defaultPairDataKey)
  protected override val defaultValue = BooleanDataEntry(defaultPairDataKey, value = true)

  protected override def test(f: PersistentCache[AccountDataKey, DataEntry[?]] => Unit): Unit = withDb { db =>
    val caches = new LevelDbPersistentCaches(db)
    f(caches.accountDataEntries)
  }
}
