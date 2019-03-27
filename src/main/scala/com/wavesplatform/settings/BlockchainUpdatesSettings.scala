package com.wavesplatform.settings

import com.typesafe.config.Config

case class BlockchainUpdatesSettings(enable: Boolean, bootstrapServers: String, topic: String, clientId: String)

object BlockchainUpdatesSettings {
  import net.ceedubs.ficus.Ficus._

  val configPath = "waves.blockchain-updates"
  def fromConfig(c: Config): BlockchainUpdatesSettings = {
    val config = c.getConfig(configPath)

    BlockchainUpdatesSettings(
      config.as[Boolean]("enable"),
      config.as[String]("kafka.bootstrap-servers"),
      config.as[String]("kafka.topic"),
      config.as[String]("kafka.client-id")
    )
  }
}
