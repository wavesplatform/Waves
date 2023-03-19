package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.runner.storage.DiskStorage
import com.wavesplatform.state.AccountScriptInfo

class AccountScriptPersistentCacheTestSuite extends PersistentCacheTestSuite[Address, AccountScriptInfo] {
  protected override val defaultKey = alice.toAddress
  protected override val defaultValue = AccountScriptInfo(
    publicKey = alice.publicKey,
    script = Script.fromBase64String("base64:BQkAAGYAAAACBQAAAAZoZWlnaHQAAAAAAAAAAABXs1wV").explicitGet(),
    verifierComplexity = 0,
    complexitiesByEstimator = Map.empty
  )

  protected override def test(f: (DiskStorage, PersistentCache[Address, AccountScriptInfo]) => Unit): Unit = withDb { db =>
    val caches = db.readOnly(DefaultPersistentCaches(db)(_))
    f(db, caches.accountScripts)
  }
}
