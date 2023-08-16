package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.runner.caches.WeighedAssetDescription
import com.wavesplatform.ride.runner.db.{Heights, ReadOnly, RideDbAccess}
import com.wavesplatform.state.{AssetDescription, Height}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Asset, AssetIdLength}
import com.wavesplatform.utils.StringBytes

class AccountDescriptionDiskCacheTestSuite extends DiskCacheWithHistoryTestSuite[IssuedAsset, WeighedAssetDescription] {
  protected override val defaultKey = Asset.IssuedAsset(ByteStr(Array.fill[Byte](AssetIdLength)(0)))
  protected override val defaultValue = WeighedAssetDescription(
    scriptWeight = 0,
    assetDescription = AssetDescription(
      originTransactionId = defaultKey.id,
      issuer = alice.publicKey,
      name = "name".toByteString,
      description = "description".toByteString,
      decimals = 8,
      reissuable = false,
      totalVolume = 1000,
      lastUpdatedAt = Height(0),
      script = None,
      sponsorship = 0,
      nft = false,
      sequenceInBlock = 0,
      issueHeight = Height @@ 0
    )
  )

  protected override def test(f: (RideDbAccess, DiskCache[IssuedAsset, WeighedAssetDescription]) => Unit): Unit = withDb { db =>
    val caches = db.batchedReadOnly(DefaultDiskCaches(db)(_))
    f(db, caches.assetDescriptions)
  }

  override protected def getHistory(implicit ctx: ReadOnly): Heights =
    ctx
      .getOpt(KvPairs.AssetDescriptionsHistory.at(defaultKey))
      .getOrElse(Vector.empty)
}
