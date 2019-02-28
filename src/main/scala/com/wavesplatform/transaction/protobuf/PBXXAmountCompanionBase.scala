package com.wavesplatform.transaction.protobuf
import com.wavesplatform.transaction.protobuf.XXAmount.{Amount, AssetAmount}

trait PBXXAmountCompanionBase {
  implicit def fromAssetIdOptionAndAmount(v: (Option[VanillaAssetId], Long)): XXAmount = v match {
    case (Some(assetId), amount) =>
      XXAmount().withAssetAmount(AssetAmount(assetId, amount))

    case (None, amount) =>
      XXAmount().withWavesAmount(amount)
  }

  implicit def fromAssetIdAndAmount(v: (VanillaAssetId, Long)): XXAmount = {
    fromAssetIdOptionAndAmount((Option(v._1).filterNot(_.isEmpty), v._2))
  }

  implicit def fromPBAssetIdAndAmount(v: (PBAssetId, Long)): XXAmount = {
    fromAssetIdAndAmount((v._1.bytes, v._2))
  }

  implicit class XXAmountImplicitConversions(a: XXAmount) {
    def longAmount: Long = a.amount match {
      case Amount.Empty              => 0L
      case Amount.WavesAmount(value) => value
      case Amount.AssetAmount(value) => value.amount
    }

    def assetId: AssetId = a.amount match {
      case Amount.WavesAmount(_) | Amount.Empty        => AssetId.Waves
      case Amount.AssetAmount(AssetAmount(assetId, _)) => AssetId(assetId)
    }
  }

  implicit def extractXXAmountLongAmount(a: XXAmount): Long = a.longAmount
}
