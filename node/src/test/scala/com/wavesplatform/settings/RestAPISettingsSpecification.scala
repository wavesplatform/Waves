package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.test.FlatSpec
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

class RestAPISettingsSpecification extends FlatSpec {
  "RestAPISettings" should "read values" in {
    val config   = ConfigFactory.parseString("""waves {
                                               |  rest-api {
                                               |    enable: yes
                                               |    bind-address: "127.0.0.1"
                                               |    port: 6869
                                               |    api-key-hash: "BASE58APIKEYHASH"
                                               |    cors: yes
                                               |    api-key-different-host: yes
                                               |    transactions-by-address-limit = 10000
                                               |    distribution-address-limit = 10000
                                               |    evaluate-script-complexity-limit = 4000
                                               |    limited-pool-threads = 2
                                               |    heavy-request-processor-pool-threads = 7
                                               |    minimum-peers = 2
                                               |  }
                                               |}
      """.stripMargin)
    val settings = config.as[RestAPISettings]("waves.rest-api")

    settings.enable should be(true)
    settings.bindAddress should be("127.0.0.1")
    settings.port should be(6869)
    settings.apiKeyHash should be("BASE58APIKEYHASH")
    settings.cors should be(true)
    settings.apiKeyDifferentHost should be(true)
    settings.transactionsByAddressLimit shouldBe 10000
    settings.distributionAddressLimit shouldBe 10000
    settings.evaluateScriptComplexityLimit shouldBe 4000
    settings.limitedPoolThreads shouldBe 2
    settings.heavyRequestProcessorPoolThreads shouldBe Some(7)
    settings.minimumPeers shouldBe 2
  }
}
