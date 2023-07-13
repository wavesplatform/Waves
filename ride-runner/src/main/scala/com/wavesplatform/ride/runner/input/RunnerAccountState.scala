package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.account.{Alias, PublicKey}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset

/** @param data
  *   Some(Map.empty) means the data was here
  */
case class RunnerAccountState(
    assetBalances: Map[IssuedAsset, Long] = Map.empty,
    regularBalance: Option[Long] = None,
    leasing: Option[RunnerLeaseBalance] = None,
    generatingBalance: Option[Long] = None,
    data: Option[Map[String, RunnerDataEntry]] = None,
    aliases: List[Alias] = Nil,
    scriptInfo: Option[RunnerScriptInfo] = None
) {
  def balance(mayBeAssetId: Asset): Option[Long] = mayBeAssetId.fold(regularBalance)(assetBalances.get)
}

case class RunnerScriptInfo(
    publicKey: PublicKey = EmptyPublicKey,
    script: Script
)
