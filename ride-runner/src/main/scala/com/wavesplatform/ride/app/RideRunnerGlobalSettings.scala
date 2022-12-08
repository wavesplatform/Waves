package com.wavesplatform.ride.app

import com.typesafe.config.Config
import com.wavesplatform.blockchain.BlockchainProcessor
import com.wavesplatform.grpc.{DefaultBlockchainApi, GrpcChannelSettings, GrpcConnector}
import com.wavesplatform.ride.app.RideRunnerSettings.DbSettings
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
