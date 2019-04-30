package com.wavesplatform.protobuf.transaction
import com.google.protobuf.ByteString
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}

object PBAmounts {
  def fromAssetAndAmount(asset: Asset, amount: Long): Amount = asset match {
    case Asset.IssuedAsset(id) => Amount().withAssetAmount(AssetAmount(ByteString.copyFrom(id), amount))
    case Asset.Waves           => Amount().withWavesAmount(amount)
  }

  def toAssetAndAmount(value: Amount): (Asset, Long) = value.amount match {
    case Amount.Amount.WavesAmount(amount)                       => (Waves, amount)
    case Amount.Amount.AssetAmount(AssetAmount(assetId, amount)) => (IssuedAsset(assetId.toByteArray), amount)
    case Amount.Amount.Empty                                     => throw new IllegalArgumentException("Empty amount not supported")
  }
}
