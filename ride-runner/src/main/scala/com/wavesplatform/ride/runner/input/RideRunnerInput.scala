package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset.IssuedAsset
import play.api.libs.json.*

case class RideRunnerInput(
    address: Address,
    request: JsObject,
    trace: Boolean = false,
    accounts: Map[Address, RunnerAccountState] = Map.empty,
    height: Int = 3296626,
    extraFeatures: Set[Short] = Set.empty,
    assets: Map[IssuedAsset, RunnerAssetInfo] = Map.empty,
    blocks: Map[Int, RunnerBlockInfo] = Map.empty,
    transactions: Map[ByteStr, RunnerTransactionInfo] = Map.empty
)
