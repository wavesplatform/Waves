package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.database.AddressId
import com.wavesplatform.ride.runner.caches.mem.MemCacheKey
import com.wavesplatform.ride.runner.db.{Heights, ReadOnly, RideDbAccess}
import com.wavesplatform.state.{BooleanDataEntry, DataEntry}

class AccountDataDiskCacheTestSuite extends DiskCacheWithHistoryTestSuite[MemCacheKey.AccountData, DataEntry[?]] {
  private val defaultAddressId        = AddressId(0L) // There is only one addressId
  private val defaultPairDataKey      = "foo"
  protected override val defaultKey   = MemCacheKey.AccountData(alice.publicKey.toAddress, defaultPairDataKey)
  protected override val defaultValue = BooleanDataEntry(defaultPairDataKey, value = true)

  protected override def test(f: (RideDbAccess, DiskCache[MemCacheKey.AccountData, DataEntry[?]]) => Unit): Unit = withDb { db =>
    val caches = db.batchedReadOnly(DefaultDiskCaches(db)(_))
    f(db, caches.accountDataEntries)
  }

  override protected def getHistory(implicit ctx: ReadOnly): Heights =
    ctx
      .getOpt(KvPairs.AccountDataEntriesHistory.at((defaultAddressId, defaultPairDataKey)))
      .getOrElse(Vector.empty)
}
