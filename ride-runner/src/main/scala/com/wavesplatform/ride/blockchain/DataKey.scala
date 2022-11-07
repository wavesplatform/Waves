package com.wavesplatform.ride.blockchain

import com.wavesplatform.account.Address
import com.wavesplatform.state.{AssetDescription, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset

sealed trait DataKey extends Product with Serializable {
  type Value
  def reload[TagT](blockchainStorage: SharedBlockchainStorage[TagT], height: Int): Unit
}

object DataKey {
  case class AssetDescriptionDataKey(asset: IssuedAsset) extends DataKey {
    override type Value = AssetDescription
    override def reload[TagT](blockchainStorage: SharedBlockchainStorage[TagT], height: Int): Unit =
      blockchainStorage.assets.reload(height, asset)
  }

  case class PortfolioDataKey(address: Address) extends DataKey {
    override type Value = Portfolio
    override def reload[TagT](blockchainStorage: SharedBlockchainStorage[TagT], height: Int): Unit =
      blockchainStorage.portfolios.reload(height, address)
  }
}
