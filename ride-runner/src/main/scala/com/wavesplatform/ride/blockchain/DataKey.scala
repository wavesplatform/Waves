package com.wavesplatform.ride.blockchain

import com.wavesplatform.account.Address
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, DataEntry, Portfolio, TransactionId, TxMeta}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction

// TODO no need of sealed
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

  case class AccountDataDataKey(address: Address, key: String) extends DataKey {
    override type Value = DataEntry[?]
    override def reload[TagT](blockchainStorage: SharedBlockchainStorage[TagT], height: Int): Unit =
      blockchainStorage.data.reload(height, (address, key))
  }

  case class AccountScriptDataKey(address: Address) extends DataKey {
    override type Value = AccountScriptInfo

    override def reload[TagT](blockchainStorage: SharedBlockchainStorage[TagT], height: Int): Unit =
      blockchainStorage.accountScripts.reload(height, address)
  }

  case class TransactionDataKey(txId: TransactionId) extends DataKey {
    override type Value = (TxMeta, Option[Transaction])

    override def reload[TagT](blockchainStorage: SharedBlockchainStorage[TagT], height: Int): Unit =
      blockchainStorage.transactions.reload(height, txId)
  }
}
