package com.wavesplatform.settings

import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class UTXSettingsSpecification extends FlatSpec with Matchers {
  "UTXSettins" should "read values" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  utx {
        |    size: 10000
        |    broadcast-interval: 30s
        |  }
        |}
      """.stripMargin).resolve()
    val settings = UTXSettings.fromConfig(config)
    settings.size should be(10000)
    settings.broadcastInterval should be(30.seconds)
  }
}
