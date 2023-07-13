package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.account.{Alias, PublicKey}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Asset, TxNonNegativeAmount}

/** @param data
  *   Some(Map.empty) means the data was here
  */
case class RunnerAccountState(
    assetBalances: Map[IssuedAsset, TxNonNegativeAmount] = Map.empty,
    regularBalance: Option[TxNonNegativeAmount] = None,
    leasing: Option[RunnerLeaseBalance] = None,
    generatingBalance: Option[TxNonNegativeAmount] = None,
    data: Option[Map[String, RunnerDataEntry]] = None,
    aliases: List[Alias] = Nil,
    scriptInfo: Option[RunnerScriptInfo] = None
) {
  def balance(mayBeAssetId: Asset): Option[Long] = mayBeAssetId.fold(regularBalance)(assetBalances.get).map(_.value)
}

case class RunnerScriptInfo(
    publicKey: PublicKey = EmptyPublicKey,
    script: Script
)
