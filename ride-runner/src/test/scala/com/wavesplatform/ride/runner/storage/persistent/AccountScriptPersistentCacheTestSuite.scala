package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.AddressId
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.runner.db.{Heights, ReadOnly, RideDbAccess}
import com.wavesplatform.ride.runner.storage.WeighedAccountScriptInfo

class AccountScriptPersistentCacheTestSuite extends PersistentCacheWithHistoryTestSuite[Address, WeighedAccountScriptInfo] {
  private val defaultAddressId      = AddressId(0L) // There is only one addressId
  protected override val defaultKey = alice.toAddress
  protected override val defaultValue = WeighedAccountScriptInfo(
    publicKey = alice.publicKey,
    scriptInfoWeight = 0, // Doesn't matter here
    script = Script.fromBase64String("base64:BQkAAGYAAAACBQAAAAZoZWlnaHQAAAAAAAAAAABXs1wV").explicitGet(),
    verifierComplexity = 0,
    complexitiesByEstimator = Map.empty
  )

  protected override def test(f: (RideDbAccess, PersistentCache[Address, WeighedAccountScriptInfo]) => Unit): Unit = withDb { db =>
    val caches = db.batchedReadOnly(DefaultPersistentCaches(db)(_))
    f(db, caches.accountScripts)
  }

  override protected def getHistory(implicit ctx: ReadOnly): Heights =
    ctx
      .getOpt(KvPairs.AccountScriptsHistory.at(defaultAddressId))
      .getOrElse(Vector.empty)
}
