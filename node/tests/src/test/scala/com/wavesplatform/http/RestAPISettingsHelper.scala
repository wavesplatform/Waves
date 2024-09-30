package com.wavesplatform.http

import com.typesafe.config.ConfigFactory
import com.wavesplatform.api.http.`X-Api-Key`
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.settings.*
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*

trait RestAPISettingsHelper {
  private val apiKey: String = "test_api_key"

  val ApiKeyHeader = `X-Api-Key`(apiKey)

  lazy val MaxTransactionsPerRequest = 10000
  lazy val MaxAddressesPerRequest    = 10000
  lazy val MaxKeysPerRequest         = 1000
  lazy val MaxAssetIdsPerRequest     = 100

  lazy val restAPISettings = {
    val keyHash = Base58.encode(crypto.secureHash(apiKey.getBytes("UTF-8")))
    ConfigFactory
      .parseString(
        s"""waves.rest-api {
           |  api-key-hash = $keyHash
           |  transactions-by-address-limit = $MaxTransactionsPerRequest
           |  distribution-address-limit = $MaxAddressesPerRequest
           |  data-keys-request-limit = $MaxKeysPerRequest
           |  asset-details-limit = $MaxAssetIdsPerRequest
           |}
         """.stripMargin
      )
      .withFallback(ConfigFactory.load())
      .as[RestAPISettings]("waves.rest-api")
  }
}
