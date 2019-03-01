package com.wavesplatform.transaction.protobuf
import com.wavesplatform.serialization.protobuf.PBMappers

trait PBAmountImplicits { self: PBMappers =>
  implicit def fromAssetIdOptionAndAmount(v: (Option[VanillaAssetId], Long)): Amount = v match {
    case (Some(assetId), amount) =>
      Amount.defaultInstance.withAssetAmount(AssetAmount(assetId, amount))

    case (None, amount) =>
      Amount.defaultInstance.withWavesAmount(amount)
  }

  implicit def fromAssetIdAndAmount(v: (VanillaAssetId, Long)): Amount = {
    fromAssetIdOptionAndAmount((Option(v._1).filterNot(_.isEmpty), v._2))
  }

  implicit def fromPBAssetIdAndAmount(v: (PBAssetId, Long)): Amount = {
    fromAssetIdAndAmount((v._1.bytes, v._2))
  }

  implicit class AmountImplicitConversions(a: Amount) {
    def longAmount: Long = a.amount match {
      case Amount.Amount.Empty              => 0L
      case Amount.Amount.WavesAmount(value) => value
      case Amount.Amount.AssetAmount(value) => value.amount
    }

    def assetId: AssetId = a.amount match {
      case Amount.Amount.WavesAmount(_) | Amount.Amount.Empty => AssetId.Waves
      case Amount.Amount.AssetAmount(AssetAmount(assetId, _)) => AssetId(assetId.byteStr)
    }
  }

  implicit def implicitLongToAmount(a: Long): Amount = Amount.defaultInstance.withWavesAmount(a)

  implicit def extractAmountLongAmount(a: Amount): Long = a.longAmount

}
