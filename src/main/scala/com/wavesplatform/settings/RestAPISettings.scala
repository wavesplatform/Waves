package com.wavesplatform.settings
import net.ceedubs.ficus.readers.{ArbitraryTypeReader, ValueReader}
import net.ceedubs.ficus.Ficus._

case class RestAPISettings(enable: Boolean,
                           bindAddress: String,
                           port: Int,
                           apiKeyHash: String,
                           cors: Boolean,
                           apiKeyDifferentHost: Boolean,
                           transactionsByAddressLimit: Int,
                           distributionAddressLimit: Int)

object RestAPISettings {
  implicit val restAPISettingsValueReader: ValueReader[RestAPISettings] = ArbitraryTypeReader.arbitraryTypeValueReader
}
