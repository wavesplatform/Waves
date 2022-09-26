package com.wavesplatform.ride.input

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.state.InvokeScriptResult.DataEntry
import com.wavesplatform.state.{AccountScriptInfo, BalanceSnapshot, LeaseBalance}
import com.wavesplatform.transaction.Asset

case class AccountState(
    script: Option[AccountScriptInfo] = None,
    data: Map[String, DataEntry] = Map.empty,
    hasData: Option[Boolean] = None,
    balance: Map[Asset, Long] = Map.empty,
    lease: Option[LeaseBalance] = None,
    balanceSnapshots: Map[Int, Map[Option[BlockId], Seq[BalanceSnapshot]]] = Map.empty
)
