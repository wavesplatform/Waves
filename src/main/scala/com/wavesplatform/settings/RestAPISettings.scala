package com.wavesplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

case class RestAPISettings(enable: Boolean, bindAddress: String, port: Int, apiKeyHash: String, cors: Boolean)

object RestAPISettings {
  val configPath: String = "waves.rest-api"

  def fromConfig(config: Config): RestAPISettings = {
    val enable = config.as[Boolean](s"$configPath.enable")
    val bindAddress = config.as[String](s"$configPath.bind-address")
    val port = config.as[Int](s"$configPath.port")
    val apiKeyHash = config.as[String](s"$configPath.api-key-hash")
    val cors = config.as[Boolean](s"$configPath.cors")

    RestAPISettings(enable, bindAddress, port, apiKeyHash, cors)
  }
}