package com.wavesplatform.events.settings

import com.typesafe.config.Config

case class BlockchainUpdatesSettings(bootstrapServers: String, topic: String, clientId: String)

object BlockchainUpdatesSettings {
  import net.ceedubs.ficus.Ficus._

  val configPath = "waves.blockchain-updates"
  def fromConfig(c: Config): BlockchainUpdatesSettings = {
    val config = c.getConfig(configPath)

    BlockchainUpdatesSettings(
      config.as[String]("bootstrap-servers"),
      config.as[String]("topic"),
      config.as[String]("client-id")
    )
  }
}
