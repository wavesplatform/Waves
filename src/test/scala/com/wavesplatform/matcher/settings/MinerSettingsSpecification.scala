package com.wavesplatform.matcher.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.MinerSettings
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class MinerSettingsSpecification extends FlatSpec with Matchers {
  "MinerSettins" should "read values" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  miner {
        |    enable: yes
        |    offline: no
        |    quorum: 1
        |    generation-delay: 1s
        |    interval-after-last-block-then-generation-is-allowed: 1d
        |    tf-like-scheduling: yes
        |  }
        |}
      """.stripMargin).resolve()

    val settings = MinerSettings.fromConfig(config)

    settings.enable should be(true)
    settings.offline should be(false)
    settings.quorum should be(1)
    settings.generationDelay should be(1.second)
    settings.intervalAfterLastBlockThenGenerationIsAllowed should be(1.day)
    settings.tfLikeScheduling should be(true)
  }
}
