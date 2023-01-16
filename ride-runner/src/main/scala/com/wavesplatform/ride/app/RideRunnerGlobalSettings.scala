package com.wavesplatform.ride.app

import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.api.{DefaultBlockchainApi, GrpcChannelSettings, GrpcConnector, RideApi}
import com.wavesplatform.blockchain.BlockchainProcessor
import com.wavesplatform.ride.app.RideRunnerSettings.DbSettings
import com.wavesplatform.settings.*
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*
import play.api.libs.json.JsObject

case class RideRunnerGlobalSettings(
    rideRunner: RideRunnerSettings,
    blockchain: BlockchainSettings,
    restApi: RestAPISettings,
    compare: CompareSettings
)

object RideRunnerGlobalSettings {
  def fromRootConfig(config: Config): RideRunnerGlobalSettings = config.getConfig("waves").as[RideRunnerGlobalSettings]
}

case class RideRunnerSettings(
    db: DbSettings,
    processor: BlockchainProcessor.Settings,
    blockchainApi: DefaultBlockchainApi.Settings,
    grpcConnector: GrpcConnector.Settings,
    grpcApiChannel: GrpcChannelSettings,
    blockchainUpdatesApiChannel: GrpcChannelSettings
)

object RideRunnerSettings {
  case class DbSettings(directory: String)
}

case class CompareSettings(rideApi: RideApi.Settings, testRequests: Map[Address, JsObject])
