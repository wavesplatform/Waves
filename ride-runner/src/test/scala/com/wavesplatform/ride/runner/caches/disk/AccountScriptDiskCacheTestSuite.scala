package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.AddressId
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.runner.caches.WeighedAccountScriptInfo
import com.wavesplatform.ride.runner.db.{Heights, ReadOnly, ReadWrite}
import com.wavesplatform.state.AccountScriptInfo

class AccountScriptDiskCacheTestSuite extends DiskCacheWithHistoryTestSuite[Address, WeighedAccountScriptInfo] {
  private val defaultAddressId      = AddressId(0L) // There is only one addressId
  protected override val defaultKey = aliceAddr
  protected override val defaultValue = WeighedAccountScriptInfo(
    scriptInfoWeight = 0, // Doesn't matter here
    accountScriptInfo = AccountScriptInfo(
      publicKey = alice.publicKey,
      script = Script.fromBase64String("base64:BQkAAGYAAAACBQAAAAZoZWlnaHQAAAAAAAAAAABXs1wV").explicitGet(),
      verifierComplexity = 0
    )
  )

  protected override def test(f: DiskCache[Address, WeighedAccountScriptInfo] => ReadWrite => Unit): Unit = withDb { db =>
    db.directReadWrite { implicit ctx =>
      f(DefaultDiskCaches(db).accountScripts)(ctx)
    }
  }

  override protected def getHistory(implicit ctx: ReadOnly): Heights =
    ctx
      .getOpt(KvPairs.AccountScriptsHistory.at(defaultAddressId))
      .getOrElse(Vector.empty)
}
