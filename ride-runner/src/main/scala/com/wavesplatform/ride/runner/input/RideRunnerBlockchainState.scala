package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.ride.runner.input.RideRunnerInputParser.given
import com.wavesplatform.transaction.Asset.IssuedAsset
import pureconfig.ConfigReader.Result
import pureconfig.generic.ProductHint
import pureconfig.{ConfigCursor, ConfigReader, ReadsMissingKeys}
import pureconfig.generic.semiauto.deriveReader

case class RideRunnerBlockchainState(
    height: Int = 3296626,
    features: Set[Short] = BlockchainFeatures.implemented,
    accounts: Map[Address, RideRunnerAccount] = Map.empty,
    assets: Map[IssuedAsset, RideRunnerAsset] = Map.empty,
    blocks: Map[Int, RideRunnerBlock] = Map.empty,
    transactions: Map[ByteStr, RideRunnerTransaction] = Map.empty
)

object RideRunnerBlockchainState {
  // required for default args to work properly: https://github.com/pureconfig/pureconfig/issues/1673
  given ConfigReader[RideRunnerBlockchainState] = deriveReader[RideRunnerBlockchainState]
}
