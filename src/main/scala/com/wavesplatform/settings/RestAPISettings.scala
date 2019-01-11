package com.wavesplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

case class RestAPISettings(enable: Boolean,
                           bindAddress: String,
                           port: Int,
                           apiKeyHash: String,
                           cors: Boolean,
                           apiKeyDifferentHost: Boolean,
                           transactionByAddressLimit: Int,
                           distributionAddressLimit: Int)

object RestAPISettings {
  val configPath: String = "waves.rest-api"

  def fromConfig(config: Config): RestAPISettings = {
    RestAPISettings(
      enable = config.as[Boolean](s"$configPath.enable"),
      bindAddress = config.as[String](s"$configPath.bind-address"),
      port = config.as[Int](s"$configPath.port"),
      apiKeyHash = config.as[String](s"$configPath.api-key-hash"),
      cors = config.as[Boolean](s"$configPath.cors"),
      apiKeyDifferentHost = config.as[Boolean](s"$configPath.api-key-different-host"),
      transactionByAddressLimit = config.as[Int](s"$configPath.transactions-by-address-limit"),
      distributionAddressLimit = config.as[Int](s"$configPath.distribution-address-limit")
    )
  }
}
