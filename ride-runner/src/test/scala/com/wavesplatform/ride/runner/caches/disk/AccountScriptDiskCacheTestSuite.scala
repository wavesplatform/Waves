package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.AddressId
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.runner.caches.WeighedAccountScriptInfo
import com.wavesplatform.ride.runner.caches.mem.MemCacheKey
import com.wavesplatform.ride.runner.db.{Heights, ReadOnly, RideDbAccess}
import com.wavesplatform.state.AccountScriptInfo

class AccountScriptDiskCacheTestSuite extends DiskCacheWithHistoryTestSuite[MemCacheKey.AccountScript, WeighedAccountScriptInfo] {
  private val defaultAddressId      = AddressId(0L) // There is only one addressId
  protected override val defaultKey = MemCacheKey.AccountScript(alice.toAddress)
  protected override val defaultValue = WeighedAccountScriptInfo(
    scriptInfoWeight = 0, // Doesn't matter here
    accountScriptInfo = AccountScriptInfo(
      publicKey = alice.publicKey,
      script = Script.fromBase64String("base64:BQkAAGYAAAACBQAAAAZoZWlnaHQAAAAAAAAAAABXs1wV").explicitGet(),
      verifierComplexity = 0
    )
  )

  protected override def test(f: (RideDbAccess, DiskCache[MemCacheKey.AccountScript, WeighedAccountScriptInfo]) => Unit): Unit = withDb { db =>
    val caches = db.batchedReadOnly(DefaultDiskCaches(db)(_))
    f(db, caches.accountScripts)
  }

  override protected def getHistory(implicit ctx: ReadOnly): Heights =
    ctx
      .getOpt(KvPairs.AccountScriptsHistory.at(defaultAddressId))
      .getOrElse(Vector.empty)
}