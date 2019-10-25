package com.wavesplatform.transaction

import com.wavesplatform.transaction.Asset.Waves

trait TxFeeAmount {
  def fee: TxAmount
  def assetFee: (Asset, TxAmount) // TODO: Delete or rework
}

trait OnlyWavesFee extends TxFeeAmount {
  override def assetFee: (Asset, TxAmount) = (Waves, fee)
}

trait CustomAssetFee extends TxFeeAmount {
  def feeAssetId: Asset
  override def assetFee: (Asset, TxAmount) = (feeAssetId, fee)
}
