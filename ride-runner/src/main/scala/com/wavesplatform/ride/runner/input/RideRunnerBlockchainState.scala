package com.wavesplatform.ride.runner.input

import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.transaction.Asset.IssuedAsset
import net.ceedubs.ficus.Ficus.*
import pureconfig.*
import RideRunnerInputParser.*

case class RideRunnerBlockchainState(
    height: Int = 3296626,
    features: Set[Short] = BlockchainFeatures.implemented,
    accounts: Map[Address, RideRunnerAccount] = Map.empty,
    assets: Map[IssuedAsset, RideRunnerAsset] = Map.empty,
    blocks: Map[Int, RideRunnerBlock] = Map.empty,
    transactions: Map[ByteStr, RideRunnerTransaction] = Map.empty
)

object RideRunnerBlockchainState {
  def fromConfig(config: Config): RideRunnerBlockchainState = {
    val height       = ConfigSource.fromConfig(config).at("height").load[Int].getOrElse(3296626)
    val features     = ConfigSource.fromConfig(config).at("features").load[Set[Short]].getOrElse(BlockchainFeatures.implemented)
    val accounts     = ConfigSource.fromConfig(config).at("accounts").load[Map[Address, RideRunnerAccount]].getOrElse(Map.empty)
    // val assets       = ConfigSource.fromConfig(config).at("assets").load[Map[IssuedAsset, RideRunnerAsset]].getOrElse(Map.empty)
    // val blocks       = ConfigSource.fromConfig(config).at("blocks").load[Map[Int, RideRunnerBlock]].getOrElse(Map.empty)
    // val transactions = ConfigSource.fromConfig(config).at("transactions").load[Map[ByteStr, RideRunnerTransaction]].getOrElse(Map.empty)

    // ficus // TODO: remove
    // val accounts             = config.as[Option[Map[Address, RideRunnerAccount]]]("accounts").getOrElse(Map.empty)
    val assets               = config.as[Option[Map[IssuedAsset, RideRunnerAsset]]]("assets").getOrElse(Map.empty)
    val blocks               = config.as[Option[Map[Int, RideRunnerBlock]]]("blocks").getOrElse(Map.empty)
    val transactions         = config.as[Option[Map[ByteStr, RideRunnerTransaction]]]("transactions").getOrElse(Map.empty)
    RideRunnerBlockchainState(height, features, accounts, assets, blocks, transactions)
  }
}
