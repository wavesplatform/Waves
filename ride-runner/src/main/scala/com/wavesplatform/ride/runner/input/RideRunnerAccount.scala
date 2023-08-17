package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.Alias
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Asset, TxNonNegativeAmount}

/** @param data
  *   Some(Map.empty) means the data was here
  */
case class RideRunnerAccount(
    assetBalances: Map[IssuedAsset, TxNonNegativeAmount] = Map.empty,
    regularBalance: Option[TxNonNegativeAmount] = None,
    leasing: Option[RideRunnerLeaseBalance] = None,
    generatingBalance: Option[TxNonNegativeAmount] = None,
    data: Option[Map[String, RideRunnerDataEntry]] = None,
    aliases: List[Alias] = Nil,
    scriptInfo: Option[RideRunnerScriptInfo] = None
) {
  def balance(mayBeAssetId: Asset): Option[Long] = mayBeAssetId.fold(regularBalance)(assetBalances.get).map(_.value)
}
