package com.wavesplatform.ride.blockchain

import com.wavesplatform.state.AssetDescription
import com.wavesplatform.transaction.Asset.IssuedAsset

sealed trait DataKey extends Product with Serializable {
  type Value
  def reload[TagT](blockchainStorage: SharedBlockchainStorage[TagT], height: Int): Unit
}

object DataKey {
  case class AssetDescriptionDataKey(asset: IssuedAsset) extends DataKey {
    override type Value = AssetDescription
    override def reload[TagT](blockchainStorage: SharedBlockchainStorage[TagT], height: Int): Unit =
      blockchainStorage.reloadAssetDescription(height, asset)
  }
}
