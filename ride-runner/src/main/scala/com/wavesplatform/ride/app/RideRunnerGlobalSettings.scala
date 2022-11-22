package com.wavesplatform.ride.app

import com.typesafe.config.Config
import com.wavesplatform.blockchain.BlockchainProcessor
import com.wavesplatform.settings.*
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*

case class RideRunnerGlobalSettings(
    rideRunner: RideRunnerSettings,
    blockchain: BlockchainSettings,
    restApi: RestAPISettings
)

object RideRunnerGlobalSettings {
  def fromRootConfig(config: Config): RideRunnerGlobalSettings = config.getConfig("waves").as[RideRunnerGlobalSettings]
}

case class RideRunnerSettings(
    processor: BlockchainProcessor.Settings
)
