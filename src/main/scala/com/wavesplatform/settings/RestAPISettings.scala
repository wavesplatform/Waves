package com.wavesplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

final case class RestAPISettings(enable: Boolean,
                                 bindAddress: String,
                                 port: Int,
                                 apiKeyHash: String,
                                 cors: Boolean,
                                 apiKeyDifferentHost: Boolean,
                                 transactionByAddressLimit: Int,
                                 distributionAddressLimit: Int,
                                 broadcastParallelism: Int,
                                 marshallingParallelism: Int)

object RestAPISettings {
  def fromRootConfig(config: Config): RestAPISettings = {
    fromConfig(config.getConfig("waves.rest-api"))
  }

  def fromConfig(config: Config): RestAPISettings = {
    RestAPISettings(
      enable = config.as[Boolean]("enable"),
      bindAddress = config.as[String]("bind-address"),
      port = config.as[Int]("port"),
      apiKeyHash = config.as[String]("api-key-hash"),
      cors = config.as[Boolean]("cors"),
      apiKeyDifferentHost = config.as[Boolean]("api-key-different-host"),
      transactionByAddressLimit = config.as[Int]("transactions-by-address-limit"),
      distributionAddressLimit = config.as[Int]("distribution-address-limit"),
      broadcastParallelism = config.as[Int]("broadcast-parallelism"),
      marshallingParallelism = config.as[Int]("marshalling-parallelism")
    )
  }
}
