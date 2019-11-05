package com.wavesplatform.transaction

import com.wavesplatform.transaction.Asset.Waves

sealed trait TxWithFee {
  def fee: TxAmount
  def assetFee: (Asset, TxAmount) // TODO: Delete or rework
}

object TxWithFee {
  trait InWaves extends TxWithFee {
    override def assetFee: (Asset, TxAmount) = (Waves, fee)
  }

  trait InCustomAsset extends TxWithFee {
    def feeAssetId: Asset
    override def assetFee: (Asset, TxAmount) = (feeAssetId, fee)
  }
}
