package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.state.BlockchainOverrides.AccountOverrides
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Asset, TxNonNegativeAmount}
import play.api.libs.json.*

case class BlockchainOverrides(accounts: Map[Address, AccountOverrides] = Map.empty) {
  def balance(address: Address, mayBeAssetId: Asset): Option[Long] = accounts.get(address).flatMap { account =>
    mayBeAssetId.fold(account.regularBalance)(account.assetBalances.get).map(_.value)
  }
}

object BlockchainOverrides {
  import TxNonNegativeAmount.reads

  implicit val accountsMapReads: Reads[Map[Address, AccountOverrides]] =
    Reads.mapReads[Address, AccountOverrides](x => Address.fromString(x).fold(e => JsError(s"Can't parse Address in accounts: $e"), JsSuccess(_)))

  implicit val blockchainOverridesReads: Reads[BlockchainOverrides] = Json.using[Json.WithDefaultValues].reads[BlockchainOverrides]

  case class AccountOverrides(assetBalances: Map[IssuedAsset, TxNonNegativeAmount] = Map.empty, regularBalance: Option[TxNonNegativeAmount] = None)

  object AccountOverrides {
    implicit val assetBalancesMapReads: Reads[Map[IssuedAsset, TxNonNegativeAmount]] =
      Reads.mapReads[IssuedAsset, TxNonNegativeAmount](x => Asset.assetReads.reads(JsString(x)))

    implicit val accountOverridesReads: Reads[AccountOverrides] = Json.using[Json.WithDefaultValues].reads[AccountOverrides]
  }
}
