package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.account.{Alias, PublicKey}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.Asset

/** @param data
  *   Some(Map.empty) means the data was here
  */
case class RunnerAccountState(
    balance: Map[Asset, Long] = Map.empty,
    leasing: Option[RunnerLeaseBalance] = None,
    generatingBalance: Option[Long] = None,
    data: Option[Map[String, RunnerDataEntry]] = None,
    aliases: List[Alias] = Nil,
    scriptInfo: Option[RunnerScriptInfo] = None
)

case class RunnerScriptInfo(
    publicKey: PublicKey = EmptyPublicKey,
    script: Script
)
