package com.wavesplatform.settings

import akka.http.scaladsl.model.HttpMethods.*
import com.typesafe.config.ConfigFactory
import com.wavesplatform.test.FlatSpec
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*

class RestAPISettingsSpecification extends FlatSpec {
  "RestAPISettings" should "read values" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  rest-api {
        |    enable: yes
        |    bind-address: "127.0.0.1"
        |    port: 6869
        |    api-key-hash: "BASE58APIKEYHASH"
        |    cors-headers {
        |      access-control-allow-headers = [ "Authorization", "Content-Type", "X-Requested-With", "Timestamp", "Signature" ]
        |      access-control-allow-origin = "http://localhost:8080"
        |      access-control-allow-methods = ["OPTIONS", "POST", "PUT", "GET", "DELETE"]
        |      access-control-allow-credentials = yes
        |    }
        |    transactions-by-address-limit = 10000
        |    distribution-address-limit = 10000
        |    evaluate-script-complexity-limit = 4000
        |    limited-pool-threads = 2
        |    heavy-request-processor-pool-threads = 7
        |    minimum-peers = 2
        |  }
        |}
      """.stripMargin
    )
    val settings = config.as[RestAPISettings]("waves.rest-api")

    settings.enable should be(true)
    settings.bindAddress should be("127.0.0.1")
    settings.port should be(6869)
    settings.apiKeyHash should be("BASE58APIKEYHASH")
    settings.transactionsByAddressLimit shouldBe 10000
    settings.distributionAddressLimit shouldBe 10000
    settings.evaluateScriptComplexityLimit shouldBe 4000
    settings.limitedPoolThreads shouldBe 2
    settings.heavyRequestProcessorPoolThreads shouldBe Some(7)
    settings.minimumPeers shouldBe 2

    settings.corsHeaders.accessControlAllowOrigin shouldBe Some("http://localhost:8080")
    settings.corsHeaders.accessControlAllowHeaders shouldBe Seq("Authorization", "Content-Type", "X-Requested-With", "Timestamp", "Signature")
    settings.corsHeaders.accessControlAllowMethods.flatMap(getForKeyCaseInsensitive) shouldBe Seq(OPTIONS, POST, PUT, GET, DELETE)
    settings.corsHeaders.accessControlAllowCredentials shouldBe true
  }
}
