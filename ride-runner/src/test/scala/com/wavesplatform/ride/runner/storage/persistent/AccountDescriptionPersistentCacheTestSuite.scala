package com.wavesplatform.ride.runner.storage.persistent

import com.google.protobuf.UnsafeByteOperations
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.runner.db.{Heights, ReadOnly, RideDbAccess}
import com.wavesplatform.ride.runner.storage.WeighedAssetDescription
import com.wavesplatform.state.{AssetDescription, Height}
import com.wavesplatform.transaction.{Asset, AssetIdLength}

import java.nio.charset.StandardCharsets

class AccountDescriptionPersistentCacheTestSuite extends PersistentCacheWithHistoryTestSuite[Asset.IssuedAsset, WeighedAssetDescription] {
  protected override val defaultKey = Asset.IssuedAsset(ByteStr(Array.fill[Byte](AssetIdLength)(0)))
  protected override val defaultValue = WeighedAssetDescription(
    scriptWeight = 0,
    assetDescription = AssetDescription(
      originTransactionId = defaultKey.id,
      issuer = alice.publicKey,
      name = UnsafeByteOperations.unsafeWrap("name".getBytes(StandardCharsets.UTF_8)),
      description = UnsafeByteOperations.unsafeWrap("description".getBytes(StandardCharsets.UTF_8)),
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

  protected override def test(f: (RideDbAccess, PersistentCache[Asset.IssuedAsset, WeighedAssetDescription]) => Unit): Unit = withDb { db =>
    val caches = db.batchedReadOnly(DefaultPersistentCaches(db)(_))
    f(db, caches.assetDescriptions)
  }

  override protected def getHistory(implicit ctx: ReadOnly): Heights =
    ctx
      .getOpt(KvPairs.AssetDescriptionsHistory.at(defaultKey))
      .getOrElse(Vector.empty)
}