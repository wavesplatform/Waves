package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.ride.runner.storage.{AccountDataKey, Storage}
import com.wavesplatform.state.{BooleanDataEntry, DataEntry}

class AccountDataPersistentCacheTestSuite extends PersistentCacheTestSuite[AccountDataKey, DataEntry[?]] {
  private val defaultPairDataKey      = "foo"
  protected override val defaultKey   = (alice.publicKey.toAddress, defaultPairDataKey)
  protected override val defaultValue = BooleanDataEntry(defaultPairDataKey, value = true)

  protected override def test(f: (Storage, PersistentCache[AccountDataKey, DataEntry[?]]) => Unit): Unit = withDb { db =>
    val caches = db.readOnly(DefaultPersistentCaches(db)(_))
    f(db, caches.accountDataEntries)
  }
}
