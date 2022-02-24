package com.wavesplatform.transaction

import com.wavesplatform.transaction.Asset.Waves

sealed trait TxWithFee {
  def fee: TxAmount
  def assetFee: (Asset, Long) // TODO: Delete or rework
}

object TxWithFee {
  trait InWaves extends TxWithFee {
    override def assetFee: (Asset, Long) = (Waves, fee.value)
  }

  trait InCustomAsset extends TxWithFee {
    def feeAssetId: Asset
    override def assetFee: (Asset, Long) = (feeAssetId, fee.value)
  }
}
