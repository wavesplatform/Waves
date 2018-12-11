package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class RestAPISettingsSpecification extends FlatSpec with Matchers {
  "RestAPISettings" should "read values" in {
    val config   = ConfigFactory.parseString("""
        |waves {
        |  rest-api {
        |    enable: yes
        |    bind-address: "127.0.0.1"
        |    port: 6869
        |    api-key-hash: "BASE58APIKEYHASH"
        |    cors: yes
        |    api-key-different-host: yes
        |    transactions-by-address-limit = 10000
        |    distribution-address-limit = 10000
        |  }
        |}
      """.stripMargin)
    val settings = RestAPISettings.fromConfig(config)

    settings.enable should be(true)
    settings.bindAddress should be("127.0.0.1")
    settings.port should be(6869)
    settings.apiKeyHash should be("BASE58APIKEYHASH")
    settings.cors should be(true)
    settings.apiKeyDifferentHost should be(true)
    settings.transactionByAddressLimit should be(10000)
    settings.distributionAddressLimit should be(10000)
  }

}
