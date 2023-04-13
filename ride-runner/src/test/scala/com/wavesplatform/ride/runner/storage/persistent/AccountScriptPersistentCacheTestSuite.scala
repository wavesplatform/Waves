package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.AddressId
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.runner.db.{Heights, ReadOnly, RideDbAccess}
import com.wavesplatform.state.AccountScriptInfo

class AccountScriptPersistentCacheTestSuite extends PersistentCacheWithHistoryTestSuite[Address, AccountScriptInfo] {
  private val defaultAddressId      = AddressId(0L) // There is only one addressId
  protected override val defaultKey = alice.toAddress
  protected override val defaultValue = AccountScriptInfo(
    publicKey = alice.publicKey,
    script = Script.fromBase64String("base64:BQkAAGYAAAACBQAAAAZoZWlnaHQAAAAAAAAAAABXs1wV").explicitGet(),
    verifierComplexity = 0,
    complexitiesByEstimator = Map.empty
  )

  protected override def test(f: (RideDbAccess, PersistentCache[Address, AccountScriptInfo]) => Unit): Unit = withDb { db =>
    val caches = db.readOnly(DefaultPersistentCaches(db)(_))
    f(db, caches.accountScripts)
  }

  override protected def getHistory(implicit ctx: ReadOnly): Heights =
    ctx
      .getOpt(KvPairs.AccountScriptsHistory.at(defaultAddressId))
      .getOrElse(Vector.empty)
}
