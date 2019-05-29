package com.wavesplatform.protobuf.transaction
import com.google.protobuf.ByteString
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}

object PBAmounts {
  def toPBAssetId(asset: Asset): AssetId = asset match {
    case Asset.IssuedAsset(id) =>
      AssetId().withIssuedAsset(ByteString.copyFrom(id))

    case Asset.Waves =>
      AssetId().withWaves(com.google.protobuf.empty.Empty())
  }

  def toVanillaAssetId(assetId: AssetId): Asset = assetId.asset match {
    case AssetId.Asset.Waves(_)             => Waves
    case AssetId.Asset.IssuedAsset(assetId) => IssuedAsset(assetId.toByteArray)
    case _ => throw new IllegalArgumentException
  }

  def fromAssetAndAmount(asset: Asset, amount: Long): Amount =
    Amount(Some(toPBAssetId(asset)), amount)

  def toAssetAndAmount(value: Amount): (Asset, Long) =
    (toVanillaAssetId(value.getAssetId), value.amount)
}
