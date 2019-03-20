package com.wavesplatform.settings

import java.net.InetSocketAddress

import com.typesafe.config.Config

case class BlockchainUpdatesSettings(enable: Boolean, address: InetSocketAddress)

object BlockchainUpdatesSettings {
  import net.ceedubs.ficus.Ficus._

  val configPath = "waves.blockchain-updates"
  def fromConfig(c: Config): BlockchainUpdatesSettings = {
    val config = c.getConfig(configPath)

    BlockchainUpdatesSettings(
      config.as[Boolean]("enable"),
      new InetSocketAddress(config.as[String]("consumer-address"), config.as[Int]("port"))
    )
  }
}
