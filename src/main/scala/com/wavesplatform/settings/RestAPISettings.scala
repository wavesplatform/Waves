package com.wavesplatform.settings
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.{ArbitraryTypeReader, ValueReader}

case class RestAPISettings(enable: Boolean,
                           bindAddress: String,
                           port: Int,
                           apiKeyHash: String,
                           cors: Boolean,
                           apiKeyDifferentHost: Boolean,
                           transactionByAddressLimit: Int,
                           distributionAddressLimit: Int)

object RestAPISettings {
  implicit val restAPISettingsValueReader: ValueReader[RestAPISettings] = ArbitraryTypeReader.arbitraryTypeValueReader

  @deprecated("Use config.as[RestApiSettings]", "0.17.0")
  def fromRootConfig(cfg: Config): RestAPISettings = cfg.as[RestAPISettings]("waves.rest-api")
}
