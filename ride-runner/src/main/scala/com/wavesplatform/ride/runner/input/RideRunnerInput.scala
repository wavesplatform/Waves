package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.{Address, DefaultAddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.transaction.Asset.IssuedAsset
import play.api.libs.json.*

case class RideRunnerInput(
    address: Address,
    request: JsObject,
    chainId: Char = DefaultAddressScheme.chainId.toChar,
    intAsString: Boolean = false,
    trace: Boolean = false,
    evaluateScriptComplexityLimit: Int = Int.MaxValue,
    maxTxErrorLogSize: Int = 1024,
    height: Int = 3296626,
    features: Set[Short] = RideRunnerInput.DefaultFeatures,
    accounts: Map[Address, RunnerAccountState] = Map.empty,
    assets: Map[IssuedAsset, RunnerAssetInfo] = Map.empty,
    blocks: Map[Int, RunnerBlockInfo] = Map.empty,
    transactions: Map[ByteStr, RunnerTransactionInfo] = Map.empty
)

object RideRunnerInput {
  val DefaultFeatures = (1 to BlockchainFeatures.ConsensusImprovements.id).map(_.toShort).toSet
}